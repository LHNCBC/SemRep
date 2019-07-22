/* Copyright(C) 1992, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : WGRAPHS.PL							      %
%   Maintainer : Mats Carlsson						      %
%            New versions of transpose/2, reduce/2, top_sort/2 by Dan Sahlin  %
%   Updated: 3 September 1999						      %
%   Purpose: Weighted graph-processing utilities			      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*  Adapted from shared code written by Richard A O'Keefe. */

%@  This library module provides operations on weighted directed graphs.
%@  A weighted directed graph (wgraph) is represented as a list of
%@  @var{(vertex-edgelist)} pairs, where the pairs are in standard order (as
%@  produced by @code{keysort/2} with unique keys), the edgelist is a list of
%@  @var{(neighbor-weight)} pair also in standard order (as produced by
%@  @code{keysort/2} with unique keys), every weight is a nonnegative integer,
%@  and every neighbor appears as a vertex even if it has no neighbors
%@  itself.
%@  
%@  An undirected graph is represented as a directed graph where for
%@  each edge @var{(U,V)} there is a symmetric edge @var{(V,U)}.
%@  
%@  An edge @var{(U,V)} is represented as the term @var{U-V}.
%@  
%@  A vertex can be any term.  Two vertices are distinct iff they are
%@  not identical (@code{==}).
%@  
%@  A path is represented as a list of vertices.  
%@  No vertex can appear twice in a path.
%@  

:- module(wgraphs, [
	wgraph_to_ugraph/2,
	ugraph_to_wgraph/2,
	vertices_edges_to_wgraph/3,
	vertices/2,
	edges/2,
	add_vertices/3,
	del_vertices/3,
	add_edges/3,
	del_edges/3,
	transpose_wgraph/2,
	neighbors/3,
	neighbours/3,
	transitive_closure/2,
	symmetric_closure/2,
	top_sort/2,
	max_path/5,
	min_path/5,
	min_paths/3,
	path/3,
	reduce/2,
	reachable/3,
	random_wgraph/4,
	min_tree/3
   ]).

:- use_module(library(ugraphs), [
        vertices/2,
        edges/2,
	add_vertices/3,
	neighbors/3,
	neighbours/3
   ]).

:- use_module(library(ordsets), [
	ord_union/3
   ]).

:- use_module(library(heaps), [
	list_to_heap/2,
	add_to_heap/4,
	get_from_heap/4
  ]).

:- use_module(library(lists), [
	reverse/2
  ]).

:- use_module(library(avl), [
	avl_fetch/3
   ]).

:- use_module(library(random), [
	random/1
   ]).

:- use_module(library(types), [
	illarg/4
   ]).



%@  Exported predicates:
%@  
%@  @table @code

%   key_union(+KeySet1, +KeySet2, -Union)
%   is true when Union is the union of KeySet1 and KeySet2.  

key_union(Set1, [], Set) :- !, Set = Set1.
key_union([], Set2, Set2).
key_union([Head1|Tail1], [Head2|Tail2], Union) :-
	Head1 = H1-_,
	Head2 = H2-_,
	compare(Order, H1, H2),
	key_union(Order, Head1, Tail1, Head2, Tail2, Union).

key_union(<, Head1, Tail1, Head2, Tail2, [Head1|Union]) :-
	key_union(Tail1, [Head2|Tail2], Union).
key_union(=, H-I,   Tail1, H-J, Tail2, [H-K|Union]) :-
	K is min(I,J),
	key_union(Tail1, Tail2, Union).
key_union(>, Head1, Tail1, Head2, Tail2, [Head2|Union]) :-
	key_union([Head1|Tail1],  Tail2, Union).



%   key_subtract(+KeySet1, +KeySet2, -Subtract)
%   is true when Subtract is the difference between KeySet1 and KeySet2.

key_subtract(Set1, [], Set) :- !, Set = Set1.
key_subtract([], _, []).
key_subtract([Head1|Tail1], [Head2|Tail2], Subtract) :-
	Head1 = H1-_,
	Head2 = H2-_,
	compare(Order, H1, H2),
	key_subtract(Order, Head1, Tail1, Head2, Tail2, Subtract).

key_subtract(<, Head1, Tail1, Head2, Tail2, [Head1|Subtract]) :-
	key_subtract(Tail1, [Head2|Tail2], Subtract).
key_subtract(=, _,   Tail1, _, Tail2, Subtract) :-
	key_subtract(Tail1, Tail2, Subtract).
key_subtract(>, Head1, Tail1, _, Tail2, Subtract) :-
	key_subtract([Head1|Tail1],  Tail2, Subtract).



%@  @item vertices/2
%@  @itemx edges/2
%@  @itemx add_vertices/3
%@  @itemx neighbors/3
%@  @itemx neighbours/3
%@  @PLXindex {vertices/2 (wgraphs)}
%@  @PLXindex {edges/2 (wgraphs)}
%@  @PLXindex {add_vertices/3 (wgraphs)}
%@  @PLXindex {neighbors/3 (wgraphs)}
%@  @PLXindex {neighbours/3 (wgraphs)}
%@  Re-exported from @code{library(wgraphs)}.

%@  @item wgraph_to_ugraph(@var{+WeightedGraph}, @var{-Graph})
%@  @PLXindex {wgraph_to_ugraph/2 (wgraphs)}
%@  is true if @var{Graph} has the same vertices and edges as @var{WeightedGraph},
%@  except the edges of @var{Graph} are unweighted.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  wgraph_to_ugraph(WGraph, Graph) :-
%@  	(   foreach(V-WNeibs,WGraph),
%@  	    foreach(V-Neibs,Graph)
%@  	do  (   foreach(V1-_,WNeibs),
%@  		foreach(V1,Neibs)
%@  	    do  true
%@  	    )
%@  	).
%@  @end group
%@  @end example

wgraph_to_ugraph(WGraph, Graph) :-
	(   foreach(V-WNeibs,WGraph),
	    foreach(V-Neibs,Graph)
	do  (   foreach(V1-_,WNeibs),
		foreach(V1,Neibs)
	    do  true
	    )
	).


%@  @item ugraph_to_wgraph(@var{+Graph}, @var{-WeightedGraph})
%@  @PLXindex {ugraph_to_wgraph/2 (wgraphs)}
%@  is true if @var{WeightedGraph} has the same vertices and edges as @var{Graph},
%@  except the edges of @var{WeightedGraph} all have weight 1.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  ugraph_to_wgraph(Graph, WGraph) :-
%@  	(   foreach(V-Neibs,Graph),
%@  	    foreach(V-WNeibs,WGraph)
%@  	do  (   foreach(V1,Neibs),
%@  	        foreach(V1-1,WNeibs)
%@  	    do  true
%@  	    )
%@  	).
%@  @end group
%@  @end example

ugraph_to_wgraph(Graph, WGraph) :-
	(   foreach(V-Neibs,Graph),
	    foreach(V-WNeibs,WGraph)
	do  (   foreach(V1,Neibs),
	        foreach(V1-1,WNeibs)
	    do  true
	    )
	).

%@  @item ugraph_to_wgraph(+SubGraph, +WeightedGraph, -WeightedSubGraph)
%@  @PLXindex {ugraph_to_wgraph/3 (wgraphs)}
%@  is true if WeightedSubGraph has the same vertices and edges as SubGraph
%@  and the same weights as the corresponding edges in WeightedGraph.

%% [PM] 4.2.1: made determinate
ugraph_to_wgraph([], _, []).
ugraph_to_wgraph([V1-Neibs1|G1], [V2-Neibs2|G2], [V1-Neibs|G]) :-
	V1==V2, !,
	neibs_to_wneibs(Neibs1, Neibs2, Neibs),
	ugraph_to_wgraph(G1, G2, G).
ugraph_to_wgraph(G1, WeightedGraph, G) :-
	G1 = [_|_],
        WeightedGraph = [_|G2],
	ugraph_to_wgraph(G1, G2, G).

%% [PM] 4.2.1: made determinate
neibs_to_wneibs([], _, []).
neibs_to_wneibs([V1|N1], [V2-W|N2], [V2-W|N]) :-
	V1==V2, !,
	neibs_to_wneibs(N1, N2, N).
neibs_to_wneibs(N1, Neibs2, N) :-
	N1 = [_|_],
        Neibs2 = [_|N2],
	neibs_to_wneibs(N1, N2, N).


%@  @item vertices_edges_to_wgraph(@var{+Vertices}, @var{+Edges}, @var{-WeightedGraph})
%@  @PLXindex {vertices_edges_to_wgraph/3 (wgraphs)}
%@  is true if @var{Vertices} is a proper list of vertices, @var{Edges} is a proper list of
%@  edges, and @var{WeightedGraph} is a graph built from @var{Vertices} and @var{Edges}.
%@  @var{Vertices} and @var{Edges} may be in any order.  The vertices mentioned in
%@  @var{Edges} do not have to occur explicitly in @var{Vertices}.  @var{Vertices} may
%@  be used to specify vertices that are not connected to any edges.

vertices_edges_to_wgraph(Vertices0, Edges, Graph) :-
	sort(Vertices0, Vertices1),
	keysort(Edges, EdgeSet),
	(   foreach(From-(To-_),EdgeSet),
	    fromto(Bag,[From,To|S],S,[])
	do  true
	),
	sort(Bag, Vertices2),
	ord_union(Vertices1, Vertices2, VertexSet),
	group_edges(VertexSet, EdgeSet, Graph).

%@  @item del_vertices(@var{+WeightedGraph1}, @var{+Vertices}, @var{-WeightedGraph2})
%@  @PLXindex {del_vertices/3 (wgraphs)}
%@  is true if @var{WeightedGraph2} is @var{WeightedGraph1} with @var{Vertices} and all
%@  edges to and from @var{Vertices} removed from it.

del_vertices(Graph0, Vs0, Graph) :-
	sort(Vs0, Vs),
	(   foreach(V,Vs),
	    foreach(V-0,Set)
	do  true
	),
	graph_del_vertices(Graph0, Vs, Set, Graph).

graph_del_vertices(G1, [], Set, G) :- !,
	graph_del_vertices(G1, Set, G).
graph_del_vertices([], _, _, []).
graph_del_vertices([V1-N1|G1], [V2|Vs], Set, G) :-
	compare(C, V1, V2),
	graph_del_vertices(C, V1, N1, G1, V2, Vs, Set, G).

graph_del_vertices(<, V1, N1, G1, V2, Vs, Set, [V1-N|G]) :-
	key_subtract(N1, Set, N),
	graph_del_vertices(G1, [V2|Vs], Set, G).
graph_del_vertices(=, _, _, G1, _, Vs, Set, G) :-
	graph_del_vertices(G1, Vs, Set, G).
graph_del_vertices(>, V1, N1, G1, _, Vs, Set, G) :-
	graph_del_vertices([V1-N1|G1], Vs, Set, G).

graph_del_vertices([], _, []).
graph_del_vertices([V1-N1|G1], Set, [V1-N|G]) :-
	key_subtract(N1, Set, N),
	graph_del_vertices(G1, Set, G).




%@  @item add_edges(@var{+WeightedGraph1}, @var{+Edges}, @var{-WeightedGraph2})
%@  @PLXindex {add_edges/3 (wgraphs)}
%@  is true if @var{WeightedGraph2} is @var{WeightedGraph1} with @var{Edges} and their "to"
%@  and "from" vertices added to it.

add_edges(Graph0, Edges0, Graph) :-
	sort(Edges0, EdgeSet),
	(   foreach(From-(To-_),EdgeSet),
	    fromto(Vs0,[From,To|S],S,[])
	do  true
	),
	sort(Vs0, Vs),
	group_edges(Vs, EdgeSet, Graph1),
	graph_union(Graph0, Graph1, Graph).

graph_union(G0, [], G) :- !, G = G0.
graph_union([], G, G).
graph_union([V1-N1|G1], [V2-N2|G2], G) :-
	compare(C, V1, V2),
	graph_union(C, V1, N1, G1, V2, N2, G2, G).

graph_union(<, V1, N1, G1, V2, N2, G2, [V1-N1|G]) :-
	graph_union(G1, [V2-N2|G2], G).
graph_union(=, V, N1, G1, _, N2, G2, [V-N|G]) :-
	key_union(N1, N2, N),
	graph_union(G1, G2, G).
graph_union(>, V1, N1, G1, V2, N2, G2, [V2-N2|G]) :-
	graph_union([V1-N1|G1], G2, G).



%@  @item del_edges(@var{+WeightedGraph1}, @var{+Edges}, @var{-WeightedGraph2})
%@  @PLXindex {del_edges/3 (wgraphs)}
%@  is true if @var{WeightedGraph2} is @var{WeightedGraph1} with @var{Edges} removed from it.

del_edges(Graph0, Edges0, Graph) :-
	sort(Edges0, EdgeSet),
	(   foreach(From-(To-_),EdgeSet),
	    fromto(Vs0,[From,To|S],S,[])
	do  true
	),
	sort(Vs0, Vs),
	group_edges(Vs, EdgeSet, Graph1),
	graph_difference(Graph0, Graph1, Graph).


graph_difference(G0, [], G) :- !, G = G0.
graph_difference([], _, []).
graph_difference([V1-N1|G1], [V2-N2|G2], G) :-
	compare(C, V1, V2),
	graph_difference(C, V1, N1, G1, V2, N2, G2, G).

graph_difference(<, V1, N1, G1, V2, N2, G2, [V1-N1|G]) :-
	graph_difference(G1, [V2-N2|G2], G).
graph_difference(=, V, N1, G1, _, N2, G2, [V-N|G]) :-
	key_subtract(N1, N2, N),
	graph_difference(G1, G2, G).
graph_difference(>, V1, N1, G1, _, _, G2, G) :-
	graph_difference([V1-N1|G1], G2, G).


group_edges([], _, []).
group_edges([Vertex|Vertices], Edges, [Vertex-Neibs|G]) :-
	group_edges(Edges, Vertex, Neibs0, RestEdges),
	keysort(Neibs0, Neibs1),
	group_min_edges(Neibs1, Neibs),	
	group_edges(Vertices, RestEdges, G).

group_edges([V0-X|Edges], V, Neibs, RestEdges) :- V0==V, !,
        Neibs = [X|Neibs1],
	group_edges(Edges, V, Neibs1, RestEdges).
group_edges(Edges, _, [], Edges).

group_min_edges([], []).
group_min_edges([Key-W|L1], L2) :-
	group_min_edges(L1, Key, W, L2).

group_min_edges([Key1-W1|L1], Key, W, L2) :- Key1==Key, !,
	W2 is min(W1,W),
	group_min_edges(L1, Key, W2, L2).
group_min_edges(L1, Key, W, [Key-W|L2]) :-
	group_min_edges(L1, L2).



%@  @item transpose_wgraph(@var{+WeightedGraph}, @var{-Transpose})
%@  @PLXindex {transpose_wgraph/2 (wgraphs)}
%@  is true if @var{Transpose} is the graph computed by replacing each edge
%@  @var{(u,v)} in @var{WeightedGraph} by its symmetric edge @var{(v,u)}.  It can only
%@  be used one way around.  The cost is @var{O(N log N)}.

transpose_wgraph(Graph, Transpose) :-
	transpose_edges(Graph, TEdges, []),
	sort(TEdges, TEdges2),
	vertices(Graph, Vertices),
	group_edges(Vertices, TEdges2, Transpose).

transpose_edges(Graph) -->
	(   foreach(V1-Ns,Graph)
	do  (   foreach(V2-W,Ns),
		param(V1)
	    do  [V2-(V1-W)]
	    )
	).

%@  @item transitive_closure(@var{+WeightedGraph}, @var{-Closure})
%@  @PLXindex {transitive_closure/2 (wgraphs)}
%@  computes Closure as the transitive closure of @var{WeightedGraph} in
%@  @var{O(N^3)} time.  Uses Floyd's algorithm and fragments of Barney
%@  Pell's code.

transitive_closure(Graph, Closure) :-
	floyd(Graph, Graph, Closure).

floyd([], Closure, Closure).
floyd([V-_|G], E, Closure) :-
	neighbors(V, E, Y),
	floyd(E, V, Y, NewE),
	floyd(G, NewE, Closure).

floyd([], _, _, []).
floyd([X-Neibs|G], V, Y, [X-NewNeibs|NewG]) :-
	neighbors(V, Neibs, W), !,		% edge from X to V, weight W
	inc_weights(Y, W, Y1),
	key_union(Neibs, Y1, NewNeibs),
	floyd(G, V, Y, NewG).
floyd([X-Neibs|G], V, Y, [X-Neibs|NewG]) :-
	floyd(G, V, Y, NewG).

inc_weights([], _, []).
inc_weights([V-I|L1], J, [V-K|L2]) :-
	K is I+J,
	inc_weights(L1, J, L2).



%@  @item symmetric_closure(@var{+WeightedGraph}, @var{-Closure})
%@  @PLXindex {symmetric_closure/2 (wgraphs)}
%@  computes @var{Closure} as the symmetric closure of @var{WeightedGraph}, i.e.
%@  for each edge @var{(u,v)} in @var{WeightedGraph}, add its symmetric edge
%@  @var{(v,u)}.  Approx @var{O(N log N)} time.  This is useful for making a
%@  directed graph undirected.

symmetric_closure(Graph, Closure) :-
	transpose_wgraph(Graph, Transpose),
	symmetric_closure(Graph, Transpose, Closure).

symmetric_closure([], [], []).
symmetric_closure([V-Neibs1|Graph], [V-Neibs2|Transpose], [V-Neibs|Closure]) :-
	key_union(Neibs1, Neibs2, Neibs),
	symmetric_closure(Graph, Transpose, Closure).



%@  @item top_sort(@var{+Graph}, @var{-Sorted})
%@  @PLXindex {top_sort/2 (wgraphs)}
%@  finds a topological ordering of a @var{Graph} and returns the ordering
%@  as a list of @var{Sorted} vertices.  Fails iff no ordering exists, i.e.
%@  iff the graph contains cycles.  Takes @var{O(N log N)} time.

top_sort(Graph, Sorted) :-
	wgraph_to_ugraph(Graph, UGraph),
	ugraphs:top_sort(UGraph, Sorted).



%@  @item max_path(@var{+V1}, @var{+V2}, @var{+WeightedGraph}, @var{-Path}, @var{-Cost})
%@  @PLXindex {max_path/5 (wgraphs)}
%@  is true if @var{Path} is a list of vertices constituting a longest path
%@  of cost Cost from @var{V1} to @var{V2} in @var{WeightedGraph}, there being no cyclic
%@  paths from @var{V1} to @var{V2}.  Takes @var{O(N^2)} time.

max_path(Initial, Final, Graph, Path, Cost) :-
	transpose_wgraph(Graph, TGraph),
	wgraph_to_ugraph(Graph, UGraph),
	wgraph_to_ugraph(TGraph, UTGraph),
	ugraphs:max_path_init(Initial, Final, UGraph, UTGraph, UGraph2, Order),
	(   foreach(V-_,UGraph2),
	    foreach(V-([]-0), Val0)
	do  true
	),
	ugraph_to_wgraph(UGraph2, TGraph, TGraph2),
	max_path(Order, TGraph2, Val0, Val),
	ugraphs:max_path_select(Val, Path, Cost).

max_path([], _, Val, Val).
max_path([V|Order], Graph, Val0, Val) :-
	neighbors(V, Graph, Neibs),
	neighbors(V, Val0, Item),
	max_path_update(Neibs, V-Item, Val0, Val1),
	max_path(Order, Graph, Val1, Val).

%% [PM] 4.2.1: made determinate
max_path_update([], _, Val, Val).
max_path_update([N-W|Neibs], Item, [Item0|Val0], Val) :-
	Item0 = V0-(_-Cost0),
	N==V0, !,
	Item = V-(Path-Cost),
	Cost1 is Cost+W,
	(   Cost1>Cost0 -> Val = [V0-([V|Path]-Cost1)|Val1]
	;   Val = [Item0|Val1]
	),
	max_path_update(Neibs, Item, Val0, Val1).
max_path_update(Neibs, Item, XVal0, XVal) :-
	Neibs = [_|_],
        XVal0 = [X|Val0],
        XVal = [X|Val],
	max_path_update(Neibs, Item, Val0, Val).



%@  @item min_path(@var{+V1}, @var{+V2}, @var{+WeightedGraph}, @var{-Path}, @var{-Cost})
%@  @PLXindex {min_path/5 (wgraphs)}
%@  is true if @var{Path} is a list of vertices constituting a shortest path
%@  with total cost @var{Cost} from @var{V1} to @var{V2} in @var{WeightedGraph}.  Takes @var{O(N^2)}
%@  time.

% derived from Dijkstra's algorithm
min_path(Initial, Final, Graph, Path, Cost) :-
	list_to_heap([0-[Initial]], H),
	min_path(H, [], Final, Graph, Rev, Cost),
	reverse(Rev, Path).

min_path(H0, Closed0, Final, Graph, Rev, Cost) :-
	get_from_heap(H0, C0, Sofar, H1),
	Sofar = [V|_],
	(   V==Final -> Rev = Sofar, Cost = C0
	;   key_subtract([V-0], Closed0, [])
	->  min_path(H1, Closed0, Final, Graph, Rev, Cost)
	;   neighbors(V, Graph, Neibs),
	    key_subtract(Neibs, Closed0, Neibs1),
	    (   foreach(V1-X,Neibs1),
		fromto(H1,H2,H3,H),
		param([C0,Sofar])
	    do  Cost1 is X+C0,
		add_to_heap(H2, Cost1, [V1|Sofar], H3)
	    ),
	    key_union(Closed0, [V-0], Closed),
	    min_path(H, Closed, Final, Graph, Rev, Cost)
	).


%@  @item min_paths(@var{+Vertex}, @var{+WeightedGraph}, @var{-Tree})
%@  @PLXindex {min_paths/3 (wgraphs)}
%@  is true if @var{Tree} is a tree of all the shortest paths from @var{Vertex} to
%@  every other vertex in @var{WeightedGraph}.  This is the single-source
%@  shortest paths problem.  Using Dijkstra's algorithm.

min_paths(Vertex, Graph, Tree) :-
	list_to_heap([0-([]-(Vertex-0))], H),
	dijkstra(H, [], Graph, [_|Edges]),
	vertices_edges_to_wgraph([], Edges, Tree).

dijkstra(H0, Closed, Graph, Edges) :-
	get_from_heap(H0, Cost, Edge, H), !,
	dijkstra(H, Cost, Edge, Closed, Graph, Edges).
dijkstra(_, _, _, []).

dijkstra(H0, Cost, Edge, Closed0, Graph, [Edge|List]) :-
	Edge = _-(ToW),
	ToW = Vertex-_,
	\+key_subtract([ToW], Closed0, []), !,
	neighbors(Vertex, Graph, Neibs),
	key_subtract(Neibs, Closed0, Neibs1),
	(   foreach(V1-W,Neibs1),
	    fromto(H0,H1,H2,H),
	    param([Cost,Vertex])
	do  Sum is Cost+W,
	    add_to_heap(H1, Sum, Vertex-(V1-W), H2)
	),
	key_union(Closed0, [ToW], Closed),
	dijkstra(H, Closed, Graph, List).
dijkstra(H, _, _, Closed, Graph, List) :-
	dijkstra(H, Closed, Graph, List).


%@  @item path(@var{+Vertex}, @var{+WeightedGraph}, @var{-Path})
%@  @PLXindex {path/3 (wgraphs)}
%@  is given a @var{WeightedGraph} and a @var{Vertex} of that @var{WeightedGraph}, and
%@  returns a maximal @var{Path} rooted at @var{Vertex}, enumerating more @var{Paths} on
%@  backtracking.

path(Initial, Graph, Path) :-
	wgraph_to_ugraph(Graph, UGraph),
	ugraphs:path(Initial, UGraph, Path).



%@  @item reduce(@var{+WeightedGraph}, @var{-Reduced})
%@  @PLXindex {reduce/2 (wgraphs)}
%@  is true if @var{Reduced} is the reduced graph for @var{WeightedGraph}. The
%@  vertices of the reduced graph are the strongly connected
%@  components of @var{WeightedGraph}.  There is an edge in @var{Reduced} from @var{u}
%@  to @var{v} iff there is an edge in @var{WeightedGraph} from one of the
%@  vertices in @var{u} to one of the vertices in @var{v}. A strongly connected
%@  component is a maximal set of vertices where each vertex has a
%@  path to every other vertex.
%@  Algorithm from "Algorithms" by Sedgewick, page 482, Tarjan's algorithm.
%   Approximately linear in the maximum of arcs and nodes (O(N log N)).

reduce(Graph, Reduced) :-
	wgraph_to_ugraph(Graph, UGraph),
	ugraphs:strong_components(UGraph, SCCS, Map),
	(   foreach(V-Neibs,Graph),
	    foreach(V1,Vertices),
	    fromto(Edges,S0,S,[]),
	    param(Map)
	do  avl_fetch(V, Map, N1),
	    N1=node(_,_,V1),
	    (   foreach(V0-W,Neibs),
		fromto(S0,S1,S2,S),
		param([V1,Map])
	    do  avl_fetch(V0, Map, N2),
		N2=node(_,_,V2),
		(V1==V2 -> S1 = S2; S1 = [V1-(V2-W)|S2])
	    )
	),
	sort(Vertices, Vertices1),
	keysort(Edges, Edges1),
	group_edges(Vertices1, Edges1, Reduced),
	sort(SCCS, Vertices1).


%@  @item reachable(@var{+Vertex}, @var{+WeightedGraph}, @var{-Reachable})
%@  @PLXindex {reachable/3 (wgraphs)}
%@  is given a @var{WeightedGraph} and a @var{Vertex} of that @var{WeightedGraph}, and
%@  returns the set of vertices that are @var{Reachable} from that @var{Vertex}.
%@  Takes @var{O(N^2)} time.

reachable(Initial, Graph, Reachable) :-
	wgraph_to_ugraph(Graph, UGraph),
	ugraphs:reachable(Initial, UGraph, Reachable).



%@  @item random_wgraph(@var{+P}, @var{+N}, @var{+W}, @var{-WeightedGraph})
%@  @PLXindex {random_wgraph/4 (wgraphs)}
%@  where @var{P} is a probability, unifies @var{WeightedGraph} with a random
%@  graph with vertices @var{1..N} where each possible edge is included with
%@  probability @var{P} and random weight in @var{1..W}.

random_wgraph(P, N, W, WGraph) :-
	(   integer(W), W >= 1 -> true
	;   illarg(domain(integer,>=(1)),
		   random_wgraph(P,N,W,WGraph), 3, W)
	),
	ugraphs:random_ugraph(P, N, UGraph),
	(   foreach(V-UNeibs,UGraph),
	    foreach(V-WNeibs,WGraph),
	    param(W)
	do  (   foreach(Nb,UNeibs),
		foreach(Nb-Y,WNeibs),
		param(W)
	    do  random(X),
		Y is 1+integer(W*X)
	    )
	).


%@  @item min_tree(@var{+WeightedGraph}, @var{-Tree}, @var{-Cost})
%@  @PLXindex {min_tree/3 (wgraphs)}
%@  is true if @var{Tree} is a minimum-@var{Cost} spanning tree of an @emph{undirected}
%@  @var{WeightedGraph} with cost @var{Cost}, if it exists.  Using Kruskal's
%@  algorithm.

min_tree(Graph, Tree, Cost) :-
	kruskal_init(Graph, E1, Map1),
	keysort(Map1, Map2),
	keymerge(Map2),
	length(Graph, N),
	keysort(E1, E2),
	kruskal(N, E2, E3, 0, Cost),
	vertices_edges_to_wgraph([], E3, Tree).

kruskal_init([], [], []).
kruskal_init([V-Neibs|Graph], Edges, Map) :-
	kruskal_init(Neibs, V, Graph, Edges, Map).

kruskal_init([B-W|Neibs], A, Graph, [W-f(A,B,U,V)|Edges], [A-U,B-V|Map]) :-
	A @> B, !,
	kruskal_init(Neibs, A, Graph, Edges, Map).
kruskal_init(_, _, Graph, Edges, Map) :-
	kruskal_init(Graph, Edges, Map).

kruskal(1, _, [], Cost, Cost) :- !.
kruskal(N, [W-f(A,B,U,V)|Queue], Edges, Cost0, Cost) :-
	(   U==V -> kruskal(N, Queue, Edges, Cost0, Cost)
	;   U=V,
	    Edges = [A-(B-W),B-(A-W)|Edges1],
	    M is N-1,
	    Cost1 is Cost0+W,
	    kruskal(M, Queue, Edges1, Cost1, Cost)
	).

keymerge([]).
keymerge([A-X|Map]) :- keymerge(Map, A, X).

keymerge([A0-X|Map], A, X) :- A0==A, !,
	keymerge(Map, A, X).
keymerge(Map, _, _) :- keymerge(Map).

% TODO:
% keymerge -> clump_* ?
% finding all MSTs

%@  @end table
