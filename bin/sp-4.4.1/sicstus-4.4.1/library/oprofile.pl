:- module(oprofile, []).

foreign_resource(oprofile, [
    init(oprofile_init),
    deinit(oprofile_deinit)
]).

:- load_foreign_resource(system(oprofile)).

