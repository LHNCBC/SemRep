/* Copyright(C) 1988, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Name: queues.pl                                                          %
%  Maintainer: Lena Flood                                                   %
%  Date: 8 November 1988                                                    %
%  Purpose: Queue operations package                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(queues3, [
        empty_queue/1,
        is_queue/1,
        queue/2,
        queue_head/3,
        queue_head_list/3,
        queue_last/3,
        queue_last_list/3,
        list_queue/2,
        queue_length/2
	]).

:- use_module(library(queues), [
        empty_queue/1,
        is_queue/1,
        singleton_queue/2,
        queue_cons/3,
        append_queue/3,
        % queue_last/3,
        queue_append/3,
        list_queue/2,
        queue_length/2
	]).

queue(Item, Queue) :-
	singleton_queue(Item, Queue).

queue_head(Item, Queue1, Queue2) :-
	queue_cons(Item, Queue1, Queue2).

queue_head_list(List, Queue1, Queue2) :-
	append_queue(List, Queue1, Queue2).

queue_last(Last, Queue1, Queue2) :-
	queues:queue_last(Queue1, Last, Queue2).

queue_last_list(List, Queue1, Queue2) :-
	queue_append(Queue1, List, Queue2).

