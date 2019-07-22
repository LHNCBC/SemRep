:- module(perf, []).

foreign_resource(perf, [
    init(perf_init),
    deinit(perf_deinit)
]).

:- load_foreign_resource(system(perf)).

