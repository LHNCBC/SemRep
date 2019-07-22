%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% COMPILATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_for_multiple_arities :-
	setof(FileName-Pred:Arities,
	      multiple_arity_predicate(FileName, Pred, Arities),
	      MultipleArityPredicates),
	!,
	nl,
	warn_multiple_arity_predicates(MultipleArityPredicates),
	nl.
check_for_multiple_arities.

multiple_arity_predicate(AbsoluteFileName, PredicateName, Arities) :-
	gen_pred_name(PredicateName, AbsoluteFileName, Predicate),
	functor(Predicate, PredicateName, _Arity),
	setof(Term, gen_pred_with_known_functor(PredicateName, AbsoluteFileName, Term), PredList),
	map_preds_to_arities(PredList, MultipleArities),
	sort(MultipleArities, Arities),
	Arities = [_,_|_].

gen_pred_name(PredicateName, FileName, SkeletalSpecification) :-
	source_file(SkeletalSpecification, FileName),
	functor(SkeletalSpecification, PredicateName, _).
gen_pred_name(PredicateName, FileName, SkeletalSpecification) :-
	source_file(_Module:SkeletalSpecification, FileName),
	functor(SkeletalSpecification, PredicateName, _).

gen_pred_with_known_functor(PredicateName, FileName, SkeletalSpecification) :-
	current_predicate(PredicateName,  SkeletalSpecification),
	source_file(SkeletalSpecification, FileName).
gen_pred_with_known_functor(PredicateName, FileName, SkeletalSpecification) :-
	current_predicate(PredicateName,  _Module:SkeletalSpecification),
	source_file(_Module:SkeletalSpecification, FileName).

map_preds_to_arities([], []).
map_preds_to_arities([Pred1|RestPreds], [Arity1|RestArities]) :-
	functor(Pred1, _Functor, Arity1),
	map_preds_to_arities(RestPreds, RestArities).
	
warn_multiple_arity_predicates([]).
warn_multiple_arity_predicates([FileName-Pred:ArityList|Rest]) :-
	( sub_atom(specialist, FileName) ->
	  basename(FileName, BaseName),
	  format(user_output, '~N~q/~w in ~w~n', [Pred, ArityList, BaseName])
	; true
	),
	warn_multiple_arity_predicates(Rest).
