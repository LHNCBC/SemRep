/* -*- Mode:Prolog; buffer-read-only:t -*-
  DO NOT EDIT! THIS CODE IS GENERATED
*/
:- module(timeout, []).
foreign(to_start_timer_a, 'tart_timer_a'(+term, +term, [-integer])).
foreign(to_stop_timer_a, 'top_timer_a'([-term])).
foreign(to_timer_now, 'imer_now'([-term])).
foreign(to_clocks_per_second, 'locks_per_second'([-integer])).
foreign_resource(timeout, [
        init(to_init),
        deinit(to_deinit),
        to_start_timer_a, to_stop_timer_a, to_timer_now,
        to_clocks_per_second
                          ]).
