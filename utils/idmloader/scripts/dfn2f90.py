import yaml
from pathlib import Path

def parse_mf6var_file(fname):
    f = open(fname, 'r')
    lines = f.readlines()
    f.close()

    vardict = {}
    vd = {}

    for line in lines:

        # skip blank lines
        if len(line.strip()) == 0:
            if len(vd) > 0:
                name = vd['name']
                if 'block' in vd:
                    block = vd['block']
                    key = (name, block)
                else:
                    key = name
                if name in vardict:
                    raise Exception(
                        'Variable already exists in dictionary: ' + name)
                vardict[key] = vd
            vd = {}
            continue

        # skip comments
        if '#' in line.strip()[0]:
            continue

        ll = line.strip().split()
        if len(ll) > 1:
            k = ll[0]
            istart = line.index(' ')
            v = line[istart:].strip()
            if k in vd:
                raise Exception('Attribute already exists in dictionary: ' + k)
            vd[k] = v

    if len(vd) > 0:
        name = vd['name']
        if 'block' in vd:
            block = vd['block']
            key = (name, block)
        else:
            key = name
        if name in vardict:
            raise Exception(
                'Variable already exists in dictionary: ' + name)
        vardict[key] = vd
    return vardict


def get_blocknames(var_dict):
    blocknames = []
    for var, bname in var_dict:
        if bname not in blocknames:
            blocknames.append(bname)
    return blocknames


def construct_f90_statement(tuple_list):
    f90statement = f"    InputDefinitionType( &\n"
    for i, (value, varname) in enumerate(tuple_list):
        comma = ','
        if i + 1 == len(tuple_list):
            comma = ''
        v = f"'{value}'"
        if value in ['.false.', '.true.']:
            v = f"{value}"
        f90statement += f"      {v}{comma} &   ! {varname}\n"
    f90statement += f"    ), &"
    return f90statement


def output_block(blockname, var_dict, component, subcomponent):

    # comment
    s = f"\n    ! {component} {subcomponent} {blockname.upper()}\n"

    r = '.true.'
    if blockname.upper() == 'OPTIONS':
        r = '.false.'

    # todo: need to replace fortran variable with STRUCTARRAY DATA1, DATA2, ...
    tuple_list = [(component, 'component'),
                  (subcomponent, 'subcomponent'),
                  (blockname.upper(), 'block'),
                  (blockname.upper(), 'tag name'),
                  ('--', 'fortran variable'),
                  ('BLOCK', 'type'),
                  ('', 'shape'),
                  (r, 'required'),
                  ('.false.', 'multi-record')]
    s += construct_f90_statement(tuple_list) + "\n"

    for k in var_dict:

        varname, bname = k
        if bname !=  blockname:
            continue

        v = var_dict[k]
        c = component
        sc = subcomponent
        b = v['block'].upper()

        # variable name
        vn = v['name'].upper()
        mf6vn = vn
        if 'mf6internal' in v:
            mf6vn = v['mf6internal'].upper()

        t = v['type'].upper()

        shape = ''
        shapelist = []
        if 'shape' in v:
            shape = v['shape']
            shape = shape.replace("(","")
            shape = shape.replace(")","")
            shape = shape.upper()
            shapelist = shape.strip().split()
        ndim = len(shapelist)

        if t == "DOUBLE PRECISION":
            t = "DOUBLE"
        if shape != '':
            t = f"{t}{ndim}D"

        r = ".true."
        if 'optional' in v:
            if v['optional'] == 'true':
                r = '.false.'
            else:
                r = '.true.'

        inrec = ".false."
        if 'in_record' in v:
            if v['in_record'] == 'true':
                inrec = ".true."
            else:
                inrec = ".false."

        tuple_list = [(c, 'component'), (sc, 'subcomponent'),
                      (b, 'block'), (vn, 'tag name'), (mf6vn, 'fortran variable'),
                      (t, 'type'), (shape, 'shape'), (r, 'required'), (inrec, 'multi-record')]

        s += construct_f90_statement(tuple_list) + "\n"

    return s

def source_header(component, subcomponent):
    s = f"module {component.title()}{subcomponent.title()}InputModule" + "\n"
    s += "  use InputDefinitionModule, only: InputDefinitionType" + "\n"
    s += "  private" + "\n"
    s += f"  public {component.lower()}_{subcomponent.lower()}_definitions" + "\n"
    s += f"  type(InputDefinitionType), parameter :: {component.lower()}_{subcomponent.lower()}_definitions(*) = &" + "\n"
    s += "  [ &" + "\n"
    return s


def source_footer(component, subcomponent):
    s = "  ]" + "\n"
    s += f"end module {component.title()}{subcomponent.title()}InputModule" + "\n"
    return s


def vardict_to_yaml(var_dict):
    d = {}
    for k in var_dict:
        varname = k[0]
        blockname = k[1]

        if blockname not in d:
            d1 = {}
            d[blockname] = d1

        if varname not in d1:
            d2 = {}
            d1[varname] = d2

    for k in var_dict:
        varname = k[0]
        blockname = k[1]
        dvar = var_dict[k]

        if 'block' in dvar:
            del dvar['block']

        if 'name' in dvar:
            del dvar['name']

        d[blockname][varname] = dvar

    return d

if __name__ == '__main__':

    # this is not done yet!  Couple immediate things:
    #  1. sim-tdis requires manual modification of the PERIODDATA entry to be:
    #    InputDefinitionType( &
    #      'SIM', &   ! component
    #      'TDIS', &   ! subcomponent
    #      'PERIODDATA', &   ! block
    #      'PERIODDATA', &   ! tag name
    #      'STRUCTARRAY PERLEN NSTP TSMULT', &   ! fortran variable
    #      'BLOCK', &   ! type
    #      'NPER', &   ! shape
    #      .true., &   ! required
    #      .false. &   ! multi-record
    #    ), &
    #  2. The last comma in the generated fortan code must be deleted.  This script needs
    #     to be modified so that it doesn't add the comma for the last entry.

    fname = Path("../../../doc/mf6io/mf6ivar/dfn", "gwf-npf.dfn")
    print("Processing dfn: ", fname)
    component, subcomponent = fname.stem.upper().split("-")
    print(f"component={component}; subcomponent={subcomponent}")
    var_dict = parse_mf6var_file(fname)

    blocknames = get_blocknames(var_dict)

    # this is experimental crap to convert definition info to yaml.
    # Should try toml as an alternative.
    #newd = vardict_to_yaml(var_dict)
    #print(newd)
    #print(yaml.dump(newd))

    fname = Path('../src/input_definition', f"{component.lower()}{subcomponent.lower()}.f90")
    with open(fname, 'w') as f:

        f.write(source_header(component, subcomponent))
        for blockname in blocknames:
            print("  processing blockname => ", blockname)
            s = output_block(blockname, var_dict, component, subcomponent)
            f.write(s)
        f.write(source_footer(component, subcomponent))

    print("done...")
