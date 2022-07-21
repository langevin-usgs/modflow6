idmloader_sources = files(
    'src' / 'idmloader.f90',
    'src' / 'inputdefinition.f90',
    'src' / 'inputdefinitionselector.f90',
    'src' / 'loadfromfile.f90',
    'src' / 'structarray.f90',
    'src' / 'structvector.f90',
    'src' / 'input_definition' / 'simtdis.f90',
    'src' / 'input_definition' / 'gwfdis.f90',
    'src' / 'input_definition' / 'gwfnpf.f90',
)

executable('idmloader',
           idmloader_sources,
           link_with: mf6core, 
           install: true,
           fortran_args: util_args)
