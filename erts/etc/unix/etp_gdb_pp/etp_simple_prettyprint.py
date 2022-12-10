import gdb
import re 

unquoted_atom_re = re.compile(u'^[a-zß-öø-ÿ][a-zA-Zß-öø-ÿ0-9@_]*$')
def Eterm(): return gdb.lookup_type("Eterm")
def EtermPtr(): return Eterm().pointer()
def EtermPtrPtr(): return EtermPtr().pointer()
def ErtsCodeInfo(): return gdb.lookup_type("ErtsCodeInfo")
def ErtsCodeInfoPtr(): return ErtsCodeInfo().pointer()
def Process(): return gdb.lookup_type("Process")
def ProcessPtr(): return Process().pointer()
def ProcessPtrPtr(): return ProcessPtr().pointer()
def Atom(): return gdb.lookup_type("Atom")
def Range(): return gdb.lookup_type("Range")

def atomic_value(valobj):
    value = valobj['counter']
    return value

def etp_arch_bits():
    return global_var('etp_arch_bits')
def etp_big_endian():
    return global_var('etp_endianness') > 0
def erts_atom_table():
    return global_var('erts_atom_table')
def global_var(name):
    try:
        return gdb.lookup_global_symbol(name).value()
    except:
        return gdb.lookup_global_symbol(name)

def is_nil(value):
    ## We handle both -5 and 0x3b as NIL values so that this script
    ## works with more versions
    return value == -5 or value == 0x3b
def atom_tab(valobj):
    idx = valobj
    seg = erts_atom_table()['seg_table']
    slot = (seg+(idx >> 16)).dereference()
    entry =(slot+(idx >> 6 & 0x3FF)).dereference()
    return entry.cast(Atom().pointer())

def quoted_name(name, quote):
    return quote + ''.join(map(lambda c: quoted_char(c, quote), name)) + quote

def quoted_char(c, quote):
    point = ord(c)
    if c == quote:
        return '\\' + quote
    elif point == 0x08:
        return '\\b'
    elif point == 0x09:
        return '\\t'
    elif point == 0x0A:
        return '\\n'
    elif point == 0x0B:
        return '\\v'
    elif point == 0x0C:
        return '\\f'
    elif point == 0x0D:
        return '\\e'
    elif point >= 0x20 and point <= 0x7E or point >= 0xA0:
        return c
    elif (point > 0xFF):
        return '#NotChar<%#x>' % c
    else:
        return '\\%03o' % point

def erts_proc():
    return global_var('erts_proc')

def etp_halfword():
    return False

def the_active_code_index():
    return global_var('the_active_code_index')
# class Eterm:
#     value = 0
#     type = "Eterm" #generic type
#     children = []
#     def __init__(eterm_value, a_type="Eterm", a_children=[]):
#         value = int(eterm_value)
#         type = a_type
#         children = a_children

class SimpleEtermPrinter:
    code_pointers = {}
    eterm_type = None # immediate, string, list, map, ref ...
    hint = 'string' #'array' 'map' 'string'  #populated by 
    eterm_children = []
    def __init__(self, val):
        self.val = val # the eterm number
        #self.eterm = val # the eterm number
        self.eterm_children = [] # populated by self.etp() 
        self.eterm_type = self.etp(val) # always a string
        names = ['beam_run_process', 'beam_normal_exit', 'beam_exit',
            #'beam_save_calls',
            #'beam_bif_export_trap',
            #'beam_export_trampoline',
            'beam_continue_exit',
            'beam_return_to_trace', 'beam_return_trace', 'beam_exception_trace',
            'beam_return_time_trace']
        for name in names:
            self.code_pointers[global_var(name)] = name

    def to_string(self):
        return self.eterm_type
    def display_hint(self):
        return self.hint
    def children(self):
        if self.hint == 'array':
            for i, item in enumerate(self.eterm_children):
                yield str(i), item
        if self.hint == 'map':
            for i in range(len(self.eterm_children)):
                entry = self.eterm_children[i]
                key = entry['first']
                value = entry['second']
                yield str(key), value

    def eterm_pretty_printer(val):
        if str(val.type) == 'Eterm' or\
           str(val.type) == 'ErtsTracer':
           return SimpleEtermPrinter(val)
        return None
    def etp(self, val):
        return self.eterm(val)
        
    ############################################
    ## Print an eterm
    ############################################
    def strip_literal_tag(self, valobj):
        if valobj.type.sizeof == 4:
            # This is a 32-bit executable. There are no literal tags.
            return valobj
        
        primary_tag = valobj & 0x03
        if (primary_tag == 1 or primary_tag == 2) and valobj & 0x04:
            valobj = valobj - 0x04
        return valobj

    def eterm(self, valobj, depth = float('inf')):
        val = valobj
        valobj = self.strip_literal_tag(valobj)
        tag = val & 0x3
        if tag == 0x1:
            return self.cons(valobj, depth)
        elif tag == 0x2:
            return self.boxed(valobj, depth)
        elif tag == 0x3:
            return self.imm(valobj)
        elif val == 0x0:
            return '<the non-value>'
        elif val == 0x4:
            return '<the non-value debug>'
        else:
            return self.cp(valobj)

    def cons(self, valobj, depth = float('inf')):
        items = []
        cdr = self.strip_literal_tag(valobj)
        improper = False
        ptr = (cdr - 1).cast(EtermPtr())
        try:
            ptr.referenced_value()
        except:
            return "#ConsError<%x>" % cdr
        while True:
            cdr = self.strip_literal_tag(cdr)
            ptr = (cdr-1).cast(EtermPtr())
            items.append(ptr.dereference())
            cdr = (ptr+1).dereference()
            if is_nil(cdr):
                break
            if cdr & 0x1 == 0:
                improper = True
                break
        if improper:
            return '#ImproperList'
        ## Try to print as ascii first
        chars = ''
        isprintable = True
        for car in items:
            if car & 0xF == 0xF:
                if car >> 4 == 10:
                    chars += '\\n'
                elif car >> 4 == 9:
                    chars += '\\t'
                else:
                    chars += f'{int(car >> 4):c}'
            else:
                isprintable = False
                break
        isprintable = isprintable and chars.isprintable()
        if isprintable:
            self.hint == 'string'
            return F'"{chars}"'
        self.hint = 'array'
        ## If not printable, we print the objects
        self.eterm_children = items
        return F"[{len(items)}]"

    def etp_ref(self, valobj):
        if (valobj & 0x3) != 0x2:
            return "#NotBoxed<%x>" % valobj
        else:
            ret_str = ""
            etp_ref_header = global_var("etp_ref_header")
            etp_magic_ref_header = global_var("etp_magic_ref_header")
            etp_ptr_mask = global_var("etp_ptr_mask")
            def ErtsORefThingPtr(): return gdb.lookup_type("ErtsORefThing").pointer()
            def ErtsMRefThingPtr(): return gdb.lookup_type("ErtsMRefThing").pointer()
            etp_ref_1 = (valobj & etp_ptr_mask).cast(ErtsORefThingPtr())
            if etp_ref_1['header'] & 0x3b != 0x10:
                return "#NotRef<%x>" % etp_ref_1['header']
            else:
                if etp_ref_1['header'] != etp_ref_header and etp_ref_1['header'] != etp_magic_ref_header:
                    return "#InternalRefError<%x>" % valobj
                else:
                    etp_magic_ref = 0
                    etp_ref_1_i = 3
                    etp_ref_1_nump = 0 #(Uint32*)
                    if etp_ref_header == etp_magic_ref_header:
                        if etp_ref_1['marker'] != 0xffffffff:
                            etp_magic_ref = 1
                    else:
                        if etp_ref_1['header'] == etp_magic_ref_header:
                           etp_magic_ref = 1
                    if etp_magic_ref == 0:
                        etp_ref_1_nump = etp_ref_1['num']
                    else:
                        etp_ref_1_nump = etp_ref_1.cast(ErtsMRefThingPtr())['mb']['refn']
                    ret_str += "#Ref<0"
                    etp_ref_1_i -=1
                    while etp_ref_1_i >= 0:
                        ret_str += ".%u" % etp_ref_1_nump[etp_ref_1_i]
                        etp_ref_1_i-=1
                    ret_str +=  ">"
                    return ret_str

    def boxed(self, valobj, depth = float('inf')):
        ptr = (valobj-2).cast(EtermPtr())
        try:
            boxed_hdr = ptr.dereference()
        except:
            return "#BoxedError<%x>" % int(valobj);
        boxed_hdr = ptr.dereference()
        if boxed_hdr & 0x3f == 0x00:
            arity = (boxed_hdr >> 6)
            terms = []
            for x in range(1, arity+1):
                if depth <= 1:
                    terms.append('...')
                    break
                depth -= 1
                terms.append((ptr+x).dereference())

            self.eterm_children = terms
            self.hint = 'array'
            return F"{{}}/{len(terms)}"
        if boxed_hdr & 0x3c == 0x3c:
            # is a map
            self.hint = 'map'
            def FlatmapPtr():
                return gdb.lookup_type("flatmap_t").pointer()
            if boxed_hdr & 0xc0 == 0x0:
                #ret_str = "#{Keys:"
                #ret_str += 
                self.etp(valobj.cast(FlatmapPtr()).referenced_value()['keys'], depth+1)
                if len(self.eterm_children) > 0:
                    keys = self.eterm_children
                    self.eterm_children = []
                #ret_str += " Values:{"
                #ret_str += 
                # Preferably each value potentially being an eterm should also be expandable
                # by the pretty printer.
                values = self.etp_array_1(valobj.cast(EtermPtr) + 3, depth, depth, 0, valobj.cast(FlatmapPtr()).referenced_value()['size'], '}')
                for i, key in enumerate(keys):
                    self.eterm_children.append({'first': key, 'second': values[i]})
                #ret_str += "}"
                return "#FlatMap"
            else:
                size = (((valobj.cast(EtermPtr()).referenced_value())>>(6+2+8))&0xffff)
                ret_str += "#<%x>{" % size
                if (valobj.cast(EtermPtr).referenced_value() & 0xc0) >= 0x80:
                    # head bitmap/array
                    #ret_str += 
                    self.eterm_children = self.etp_bitmap_array_1(valobj.cast(EtermPtr())+2, depth, depth, 0, size, '}')
                else:
                    # node bitmap
                    #ret_str += 
                    self.eterm_children = self.etp_bitmap_array_1(valobj.cast(EtermPtr())+1, depth, depth, 0, size, '}')
                return "#HashMap<%x>" % size # + ret_str
        boxed_type = (boxed_hdr >> 2) & 0xF
        if boxed_type == 0xC:
            return '#ExternalPid'
        if boxed_type == 0xD:
            return '#ExternalPort'
        if boxed_type == 0x2 or boxed_type == 0x3:
            return '#Bignum'
        if boxed_type == 0x6:
            return '#Float'
        if boxed_type == 0x4:
            return self.etp_ref(valobj)
        if boxed_type == 0xE:
            return '#ExternalRef'
        if boxed_type == 0x5:
            return '#Fun'
        if boxed_type == 0x8:
            return '#RefcBin'
        if boxed_type == 0x9:
            return '#HeapBin'
        if boxed_type == 0xA:
            return '#SubBin'
        return f'#Boxed<{valobj}>'

    def etp_array_1(self, valobj, depth, width, pos, size, end_char):
        # Args: Eterm* p, int depth, int width, int pos, int size, int end_char
        #
        #
        ret = []
        for i in range(0, size):
            ret.append(SimpleEtermPrinter(valobj[pos+i]))
        return ret
    def etp_bitmap_array_1(self, valobj, depth, width, pos, size, end_char):
        # Args: Eterm* p, int depth, int width, int pos, int size, int end_char
        #
        #
        ret = []
        while size != 0:
            if size & 1 != 0:
                ret.append(SimpleEtermPrinter(valobj[pos]))
                pos+=1
            size = size >> 1    
        return ret

    def imm(self,valobj):
        self.hint='string'
        val = valobj
        if (val & 0x3) != 3:
            return '#NotImmediate<%#x>' % val
        tag = val & 0xF
        if tag == 0x3:
            return self.pid(valobj)
        elif tag == 0x7:
            return self.port(valobj)
        elif tag == 0xF:
            return str(val >> 4)
        elif tag == 0xB:
            # Immediate2
            tag2 = val & 0x3F
            if tag2 == 0x0B:
                return self.atom(valobj)
            elif tag2 == 0x1B:
                return F'#Catch<{val>>6:#x}>'
            elif is_nil(valobj):
                return '[]'
        return '#UnknownImmediate<%#x>' % val

    # Continuation pointers
    def cp(self, valobj):
        mfaptr = erts_lookup_function_info(valobj)
        if mfaptr == None:
            pointer = self.code_pointers.get(valobj)
            if pointer != None:
                return '#Cp<%s>' % pointer
            else:
                return '#Cp<%#x>' % valobj
        else:
            return '#Cp<%s>' % self.mfa(mfaptr)

    # Pids and ports
    def pid(self, valobj):
        val = valobj
        if (val & 0xF) == 0x3:
            if etp_arch_bits() == 64:
                if etp_big_endian():
                    data = (val >> 35) & 0x0FFFFFFF
                else:
                    data = (val >> 4) & 0x0FFFFFFF
            else:
                data = self.pixdata2data(valobj)
            return '<0.%u.%u>' % (data & 0x7FFF, (data >> 15) & 0x1FFF)
        else:
            return '#NotPid<%#x>' % val

    def port(self, valobj):
        val = valobj
        if (val & 0xF) == 0x7:
            if etp_arch_bits() == 64 and not etp_halfword():
                if etp_big_endian():
                    data = (val >> 36) & 0x0FFFFFFF
                else:
                    data = (val >> 4) & 0x0FFFFFFF
            else:
                data = self.pixdata2data(valobj)
            return '#Port<0.%u>' % data
        else:
            return '#NotPort<%#x>' % val

    # Strings and atoms

    def atom(self, valobj):
        #val = valobj.unsigned
        val = valobj
        if (val & 0x3F) == 0x0B:
            name = self.atom_name(atom_tab(valobj))
            if unquoted_atom_re.match(name):
                return str(name)
            else:
                return quoted_name(name, "'")
        else:
            return '#NotAtom<%#x>' % val

    def atom_name(self, entry):

        name = entry['name']
        length = entry['len']
        data = name.referenced_value()
        name_str = ""
        for i in range(0,length):
            name_str += chr((name+i).referenced_value())
        return name_str

    def pixdata2data(self, valobj):
        pixdata = valobj
        proc = erts_proc()
        ro = proc['r']['o']
        pix_mask = ro['pix_mask']
        pix_cl_mask = ro['pix_cl_mask']
        pix_cl_shift = ro['pix_cl_shift']
        pix_cli_mask = ro['pix_cli_mask']
        pix_cli_shift = ro['pix_cli_shift']
        data = pixdata & ~pix_mask
        data |= (pixdata >> pix_cl_shift) & pix_cl_mask
        data |= (pixdata & pix_cli_mask) << pix_cli_shift
        return data

MI_FUNCTIONS = 13
MI_NUM_FUNCTIONS = 0
def erts_lookup_function_info(valobj):
    r = find_range(valobj)
    if r is None:
        return None
    pc = valobj
    start = r['start'].cast(EtermPtr())
    curr = (start+MI_FUNCTIONS).cast(ErtsCodeInfoPtr().pointer())
    prev = curr
    cnt = (start+MI_NUM_FUNCTIONS).dereference()
    for x in range(0, cnt):
        prev = curr
        curr = curr+1
        if pc < curr.dereference():
            return prev.dereference()['mfa']
    return None

def find_range(valobj):
    pc = valobj
    active = the_active_code_index()
    ranges = gdb.lookup_symbol("r")[0].value()
    ranges = ranges[active['counter']]
    n = ranges["n"]
    low = ranges["modules"]
    high = low+n
    range_pointer_type = Range().pointer()
    mid = ranges['mid'].cast(range_pointer_type)
    while low < high:
        start = mid['start']
        end = atomic_value(mid['end'])
        if pc < start:
            high = mid
        elif pc > end:
            low = (mid+1)
        else:
            return mid
        length = int(high - low) // Range().sizeof
        mid = (low + (length // 2))
    return None

def mfa(self, mfa):
    return '%s:%s/%d' % (self.eterm(mfa['module']),
                         self.eterm(mfa['function']),
                         mfa['arity'])


gdb.pretty_printers.append(SimpleEtermPrinter.eterm_pretty_printer)
