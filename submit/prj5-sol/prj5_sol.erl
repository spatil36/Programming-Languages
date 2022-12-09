-module(prj5_sol).
-include_lib("eunit/include/eunit.hrl").
-compile([nowarn_export_all, export_all]).

%---------------------------- Test Control ------------------------------
%% Enabled Tests
%%   comment out -define to deactivate test.
%%   alternatively, enclose deactivated tests within
%%   -if(false). and -endif. lines
%% enable all tests before submission.
%% the skeleton file is distributed with all tests are deactivated

%move this down to just before -endif when project completed
-define(test_dept_employees1, enabled).
-define(test_dept_employees2, enabled).
-define(test_dept_employees3, enabled).
-define(test_delete_employee, enabled).
-define(test_upsert_employee, enabled).
-define(test_find_employees, enabled).
-define(test_employees_req, enabled).
-define(test_employees_req_with_sort, enabled).
-define(test_employees_client_no_sort, enabled).
-define(test_employees_client_with_sort_dosort, enabled).
-define(test_employees_client_with_sort, enabled).
-define(test_employees_client_no_sort_mutate, enabled).
-define(test_employees_client_with_sort_mutate, enabled).
-define(test_employees_client_hot_reload, enabled).
-if(false).
-endif.

%% Tracing Tests: set trace_level as desired.
% trace_level == 0:  no tracing
% trace_level == 1:  function + test-name 
% trace_level == 2:  function + test-name + args + result
-define(trace_level, 0).
-if(?trace_level == 2).
  -define(test_trace(Test, F, Args, Result),
	  io:format(standard_error, "~p:~p: ~p =~n~p~n",
		    [ element(2, erlang:fun_info(F, name)), Test,
		      Args, Result])).
-elif(?trace_level == 1).
  -define(test_trace(Test, F, _Args, _Result),
	  io:format(standard_error, "~p:~p~n",
		    [ element(2, erlang:fun_info(F, name)), Test])).
-else.
  -define(test_trace(_Test, _F, _Args, _Result), true).
-endif.

%% A TestSpec is either a triple of the form { Test, Args, Result }
%% where Test is an atom describing the test, Args gives the list of
%% arguments to the function under test and Result is the expected
%% result, or a quadruple of the form { Test, Args, Result, Fn },
%% where Fn is applied to the actual Result before being compared with
%% the expected result Result.

make_tests(F, TestSpecs) ->   
    MapFn = fun (Spec) ->
		    case Spec of
			{ _Test, Args, Result } ->
			   fun () ->  
				   FResult = apply(F, Args),
				   ?test_trace(_Test, F, Args, FResult),
				   ?assertEqual(Result, FResult) 
			   end;
			{ _Test, Args, Result, Fn } ->
			    fun () ->  
				    FResult = apply(F, Args),
				    ?test_trace(_Test, F, Args, FResult),
				    ?assertEqual(Result, Fn(FResult)) 
			    end;
			_ -> 
			    Msg = io_lib:format("unknown spec ~p", [Spec]),
			    error(lists:flatten(Msg))
		    end				
	    end,
    lists:map(MapFn, TestSpecs).


%---------------------- employee Type and Data --------------------------

% we use name as a primary key for an employee.
% code can assume that any collection of employee will have most one
% having a specific name.
-record(employee, {name, age, dept, salary}).

% given a variable E which is an employee, use E#employee.dept to
% access the employee dept field.

% employee predicates
employee_has_name(Name) -> fun (E) -> E#employee.name == Name end.
employee_has_age(Age) -> fun (E) -> E#employee.age == Age end. 
employee_has_dept(Dept) -> fun (E) -> E#employee.dept == Dept end.
employee_has_salary(Salary) -> fun (E) -> E#employee.salary == Salary end.
     
% test data

-define(Tom, #employee{name=tom, age=33, dept=cs, salary=85000.00}).
-define(Joan, #employee{name=joan, age=23, dept=ece, salary=110000.00}).
-define(Bill, #employee{name=bill, age=29, dept=cs, salary=69500.00}).
-define(John, #employee{name=john, age=28, dept=me, salary=58200.00}).
-define(Sue,  #employee{name=sue, age=19, dept=cs, salary=22000.00}).
-define(Alice, #employee{name=alice, age=33, dept=cs, salary=85000.00}).
-define(Harry, #employee{name=harry, age=23, dept=ece, salary=110000.00}).
-define(Larry, #employee{name=larry, age=33, dept=cs, salary=69500.00}).
-define(Erwin, #employee{name=erwin, age=28, dept=me, salary=58200.00}).
-define(Jane, #employee{name=jane, age=19, dept=cs, salary=22000.00}).

% data used for upserts
-define(Tom1, #employee{name=tom, age=44, dept=ece, salary=185000.00}).
-define(Jane1, #employee{name=jane, age=33, dept=ece, salary=44000.00}).
-define(Joe1, #employee{name=joe, age=33, dept=me, salary=85000.00}).

-define(Employees, 
	[ ?Tom, ?Joan, ?Bill, ?John, ?Sue, ?Alice, ?Harry, ?Larry, 
	  ?Erwin, ?Jane]).
-define(SortedEmployees, 
	[ ?Alice, ?Bill, ?Erwin, ?Harry, ?Jane, ?Joan, ?John, 
	  ?Larry, ?Sue, ?Tom ]).

employees() -> ?Employees.
upsert_employees() -> [ ?Tom1, ?Jane1, ?Joe1 ].
    


%-------------------------- dept_employees1/2 ---------------------------

% #1: "10-points"
% dept_employees1(Dept, Employees): return sub-list of Employees
% having dept = Dept.
% Restriction: must be implemented using recursion without using any library 
% functions.
dept_employees1(Dept, []) -> [];
dept_employees1(Dept, [EHead|ETail]) -> if Dept == EHead#employee.dept -> [EHead | dept_employees1(Dept, ETail)];
                                        true -> dept_employees1(Dept, ETail) end.


dept_employees_test_specs() -> 
    Es = ?Employees,
    [ { cs_empty, [cs, []], [] },
      { cs_employees, [cs, Es], [ ?Tom, ?Bill, ?Sue, ?Alice, ?Larry, ?Jane ] },
      { ece_employees, [ece, Es], [ ?Joan, ?Harry ] },
      { me_employees, [me, Es], [ ?John, ?Erwin ] },
      { ce_employees, [ce, Es], [] }				
    ].

-ifdef(test_dept_employees1).
dept_employees1_test_() ->
    make_tests(fun dept_employees1/2, dept_employees_test_specs()).
-endif. %test_dept_employees1    


%-------------------------- dept_employees2/2 ---------------------------

% #2: "5-points"
% dept_employees2(Dept, Employees): return sub-list of Employees
% having dept = Dept.
% Restriction: must be implemented using a single call to lists:filter().
dept_employees2(Dept, Employees) -> lists:filter(employee_has_dept(Dept), Employees);
dept_employees2(Dept, []) -> [].

-ifdef(test_dept_employees2).
dept_employees2_test_() ->
    make_tests(fun dept_employees2/2, dept_employees_test_specs()).
-endif. %test_dept_employees2

%-------------------------- dept_employees3/2 ---------------------------

% #3: "5-points"
% dept_employees3(Dept, Employees): return sub-list of Employees
% having dept = Dept.
% Restriction: must be implemented using a list comprehension.
dept_employees3(Dept, Employees) -> [EHead || EHead <- Employees, Dept == EHead#employee.dept];
dept_employees3(Dept, []) -> [].

-ifdef(test_dept_employees3).
dept_employees3_test_() ->
    make_tests(fun dept_employees3/2, dept_employees_test_specs()).
-endif. %test_dept_employees3

%------------------------- delete_employee/2 ----------------------------

% #4: "10-points"
% Given a list Employees of employees, return sublist of Employees
% with employee with name=Name removed.  It is ok if Name does not exist.
% Hint: use a list comprehension 
delete_employee(Name, Employees) -> [EHead || EHead <- Employees, Name =/= EHead#employee.name].

%% returns list of pairs: { Args, Result }, where Args is list of
%% arguments to function and Result should be the value returned
%% by the function.
delete_employee_test_specs() -> 
    Es = ?Employees,
    [
     { delete_last, 
       [ jane, Es ], 
       [ ?Tom, ?Joan, ?Bill, ?John, ?Sue, ?Alice, ?Harry, ?Larry, ?Erwin] }, 
     { delete_intermediate,
       [ joan, Es ], 
       [ ?Tom, ?Bill, ?John, ?Sue, ?Alice, ?Harry, ?Larry, ?Erwin, ?Jane] }, 
     { delete_nonexisting, [ joe, Es ], Es }
    ].

-ifdef(test_delete_employee).
delete_employee_test_() ->
    make_tests(fun delete_employee/2, delete_employee_test_specs()).
-endif. %test_delete_employee

%--------------------------- upsert_employee/2 --------------------------

% #5: "10-points"
% Given a list Employees of employees, if Employees contains 
% an employee E1 with E1.name == E.name, then return Employees
% with E1 replaced by E, otherwise return Employees with
% [E] appended.
upsert_employee(E, []) -> [E];
upsert_employee(E, [Ehead|Etail]) -> if E#employee.name == Ehead#employee.name -> [E | Etail];
                                    true -> [Ehead | upsert_employee(E, Etail)] end.


%% returns list of pairs: { Args, Result }, where Args is list of
%% arguments to function and Result should be the value returned
%% by the function.
upsert_employee_test_specs() -> 
    Es = ?Employees,
    [ { upsert_existing_first, [?Tom1, Es], 
	[?Tom1, ?Joan, ?Bill, ?John, ?Sue, ?Alice, 
	 ?Harry, ?Larry, ?Erwin, ?Jane ] 
      },
      { upsert_existing_last, [?Jane1, Es], 
	[? Tom, ?Joan, ?Bill, ?John, ?Sue, ?Alice, 
	 ?Harry, ?Larry, ?Erwin, ?Jane1 ] 
      },
      { upsert_new, [?Joe1, Es], 
	[? Tom, ?Joan, ?Bill, ?John, ?Sue, ?Alice, 
	 ?Harry, ?Larry, ?Erwin, ?Jane, ?Joe1 ] 
      }
    ].

-ifdef(test_upsert_employee).
upsert_employee_test_() ->
    make_tests(fun upsert_employee/2, upsert_employee_test_specs()).
-endif. %test_upsert_employee

%--------------------------- find_employees/2 ---------------------------

% #6: "15-points"
% find_employees(Preds, Employees):
% Given a list Employees of employees and a list of predicates Preds
% where each predicate P in Preds has type Employee -> bool,
% return a sub-list of Employees containing those E in Employees
% for which all P in Preds return true.
% Restriction: may not use recursion.
% Hint: consider using a list comprehension with lists:all/2.
find_employees(Preds, Employees) -> [Ehead || Ehead <- Employees, lists:all(fun(P) -> P(Ehead) end, Preds)].

find_employees_test_specs() -> 
  Es = ?Employees,
  [ { cs, [ [employee_has_dept(cs)], Es ], dept_employees3(cs, Es) },
    { cs_age, [ [employee_has_dept(cs), employee_has_age(33)], Es ],
      [ ?Tom, ?Alice, ?Larry ]
    },
    { cs_age_salary, 
      [ [ employee_has_dept(cs), 
	  employee_has_age(33), 
	  employee_has_salary(85000.00)
	], Es ],
      [ ?Tom, ?Alice ]
    },
    { name, [ [employee_has_name(erwin)], Es ], [ ?Erwin ] },
    { name_age, [ [employee_has_name(erwin), employee_has_age(33)], Es ], 
      [] 
    },
    { salary, [ [employee_has_salary(69500)], Es ], [?Bill, ?Larry] },
    { salary_none, [ [employee_has_salary(69501)], Es ], [] },
    { age, [ [employee_has_age(19)], Es ], [?Sue, ?Jane] },
    { age_none, [ [employee_has_age(20)], Es ], [] }
  ].

-ifdef(test_find_employees).
find_employees_test_() ->
    make_tests(fun find_employees/2, find_employees_test_specs()).
-endif. %test_find_employees

%--------------------------- employees_req/2 ----------------------------

% #7: "15-points"
% employees_req(Req, Employees):
% Return an ok-result of the form {ok, Result, EmployeesZ} or 
% an error-result of the form {err, ErrString, Employees}.
% Specifically, when Req matches:
%   { delete, Name }:     ok-result with Result = void and EmployeesZ =
%                         delete_employee(Employee, Employees).
%   { dump }:             ok-result with Result = Employees and 
%                         EmployeesZ = Employees.
%   { find, Preds }:      ok-result with Result = 
%                         find_employees(Preds, Employees)
%                         and EmployeesZ = Employees.
%   { read, Name }:       If Preds = [employee_has_name(Name)] and
%                         [Result] = find_employees(Preds, Employees), return
%                         an ok-result with EmployeesZ = Employees; otherwise
%                         return an error-result with a suitable ErrString.
%   { upsert, Employee }: ok-result with Result = void and 
%			  EmployeesZ = upsert_employee(Employee, Employees).
%   _:                    return an error-result with a suitable ErrString.
% Hint: use io_lib:format(Format, Args) to build suitable error strings,
% for example: lists:flatten(io_lib:format("bad Req ~p", [Req]))
employees_req(Req, Employees) -> case Req of
                                    { delete, Name } -> 
                                            {ok, void, delete_employee(Name, Employees)};
                                    { dump } ->
                                            {ok, Employees, Employees};
                                    { find, Preds } ->
                                            {ok, find_employees(Preds, Employees), Employees};
                                    { read, Name } -> case find_employees([employee_has_name(Name)], Employees) of
                                                            [] -> {err, lists:flatten(io_lib:format("Person ~w not found", [Name])), Employees};
                                                            Other -> [H|_] = find_employees([employee_has_name(Name)], Employees),
                                                                     {ok,H, Employees} end;
                                    { upsert, Employee} ->
                                            {ok, void, upsert_employee(Employee, Employees)};
                                    Other ->
                                            {err, lists:flatten(io_lib:format("bad request ~p", [Req])), Employees}        
                                end.

%% map upsert_employee_test_specs into args-result pairs suitable
%% for employees_req({upsert, _}, ...).
employees_req_upsert_test_specs() ->
  [ { Test, [ {upsert, Employee}, Employees ], { ok, void, Result } } ||
    { Test, [Employee, Employees], Result } <- upsert_employee_test_specs() ].


%% map delete_employee_test_specs into args-result pairs suitable
%% for employees_req({delete, _}, ...).
employees_req_delete_test_specs() ->
  [ { Test, [ {delete, Name}, Employees ], { ok, void, Result } } ||
    { Test, [Name, Employees], Result } <- delete_employee_test_specs() ].

%% map find_employees_test_specs into args-result pairs suitable
%% for employees_req({find, _}, ...).
employees_req_find_test_specs() ->
  [ { Test, [ {find, Preds}, Employees ], { ok, Result, Employees } } ||
    { Test, [Preds, Employees], Result } <- find_employees_test_specs() ].

ignore_err_message(Result) ->
    case Result of
      { Status, _Msg } -> { Status };
      { Status, _Msg, Rest } -> { Status, Rest }
    end.

employees_req_test_specs() ->
    % since these specs are used also by server, keep mutable tests last
    Es = ?Employees,
    [ { read_intermediate, [{ read, joan }, Es ], { ok, ?Joan, Es } },
      { read_last, [ { read, jane }, Es ], { ok, ?Jane, Es } },
      { dump, [ {dump}, Es ], { ok, Es, Es } },
      { read_nonexiting, [ { read, gary }, Es ], {err, Es}, 
	fun ignore_err_message/1 },
      { bad_req, [ { read1, joan }, Es ], {err, Es}, fun ignore_err_message/1 }
    ] ++
    employees_req_find_test_specs() ++
    employees_req_upsert_test_specs() ++
    employees_req_delete_test_specs().

-ifdef(test_employees_req).
employees_req_test_() ->
    make_tests(fun employees_req/2, employees_req_test_specs()).
-endif. %test_employees_req

%---------------------- employees_req_with_sort/2 -----------------------

% #8: "10-points"
% employees_req_with_sort(Req, Employees):
% Exactly like employees_req/2 except that it handles an additional Req:
% { sort } which should return { ok, void, SortedEmployees }
% where SortedEmployees is Employees sorted in ascending order by name.
% Hint: use lists:sort/2 to sort, delegate all non-sort Fns to employees_req/2.

employees_req_with_sort(Req, Employees) -> case Req of
                                             { sort } -> 
                                                    {ok, void, lists:sort(fun(A, B) -> A#employee.name < B#employee.name end, Employees)};
                                             Other -> 
                                                    employees_req(Req, Employees)
                                            end.

employees_req_with_sort_test_specs() ->
    [ { sort, [{sort}, ?Employees], { ok, void, ?SortedEmployees } } ] ++
    employees_req_test_specs().

% the additional specs here are not suitable for use by the server
employees_req_with_sort_extra_test_specs() ->
    [ { sort_single, [{sort}, [?Tom]], { ok, void, [?Tom]} },
      { sort_empty, [{sort}, [] ], { ok, void, [] } }
    ] ++ employees_req_with_sort_test_specs().
    
-ifdef(test_employees_req_with_sort).
employees_req_with_sort_test_() ->
    Fn = fun employees_req_with_sort/2,
    make_tests(Fn, employees_req_with_sort_extra_test_specs()).
-endif. %test_employees_req_with_sort

%-------------------- Hot Reload Server and Client ----------------------

% #9: "20-points"

% start_employees_server(Employees, Fn):
% start a server process with employees list Employees and processing
% function Fn (usually either employees_req/2 or
% employees_req_with_sort/2).  Register server process under ID emps
% and return PID of started process.
% 
% The server should accept messages of the form { Pid, Req } where Pid
% is the client's PID.  The action taken by the server depends on Req:
% { stop }:            Terminate the server after sending a { ok, stopped}
%		                    response to the client.   
% { new_fn, Fn1 }:     Continue server with processing function
%                      Fn replaced by Fn1 after sending a {ok, void} 
%                      response to the client. 
%  All other requests: Req should be forwarded to Fn as Fn(Req, Employees),
%                      where Fn is the current processing function 
%                      and Employees are the current employees in the 
%                      server state.  Assuming the result of the forwarded
%                      call is { Status, Result, Employees1 }, then the server
%                      should continue with (possibly) new Employees1 and
%                      current processing function Fn after sending a
%                      { Status, Result } response to the client.
% The actual messages returned to the client should always include the
% server's PID, so they look like { self(), Response } where Response is
% the response described above.
start_employees_server(Employees, Fn) -> register(emps, spawn(prj5_sol, employees_server, [Employees, Fn])),
                                         whereis(emps).

employees_server(Employees, Fn) -> 
                                receive
                                    {Client, {stop}} -> Client ! {ok, stopped};
                                    {Client, {new_fn, Fn1} } -> 
                                                Client ! {self(), {ok, void}},
                                                employees_server(Employees, Fn1);
                                    {Client, Req} -> 
                                                {Status, Result, Employees1} = apply(Fn, [Req, Employees]),
                                                        Client ! {self(), {Status, Result}},
                                                        employees_server(Employees1, Fn)
                                end.

% stop previously started server with registered ID emps.
% should return {ok, stopped}.
stop_employees_server() ->
  emps ! {self(), {stop}},
  receive
    {Status, Response} -> {Status, Response}
  end.

% set request Req to server registered under ID emps and return 
% Result from server.
employees_client(Req) -> 
%  ServerPid = whereis(emps),
  emps ! {self(), Req},
  receive
    {_ServerPid, {Status, Result}} ->
        {Status, Result}
end.


%% map employees_req test to a employees_client test
make_employees_client_test_specs(Specs) ->
    DropLast = fun (Tuple) -> 
		       if tuple_size(Tuple) =:= 2 ->
			  { element(1, Tuple) };
			  true -> { element(1, Tuple), element(2, Tuple) }
		       end
	       end,
    MapFn = fun (Spec) ->
		case Spec of
		    { Test, [Arg0|_], Result } -> 
			{ Test, [Arg0], DropLast(Result) };
		    { Test, [Arg0|_], Result, Fn } ->
			{ Test, [Arg0], DropLast(Result), Fn }
		end
	    end,
    [ MapFn(Spec) || Spec <- Specs ].

employees_client_no_sort_test_specs() ->
    make_employees_client_test_specs(employees_req_test_specs()).
    
employees_client_with_sort_test_specs() ->
    make_employees_client_test_specs(employees_req_with_sort_test_specs()).
    
-ifdef(test_employees_client_no_sort).
employees_client_no_sort_test_() ->
    Es = ?Employees,
    { setup,
      fun () -> start_employees_server(Es, fun employees_req/2) end,
      fun (_) ->  stop_employees_server() end,
      make_tests(fun employees_client/1, employees_client_no_sort_test_specs())
    }.
-endif.

-ifdef(test_employees_client_with_sort).
% test non-sort functionality
employees_client_with_sort_test_() ->
    Es = ?Employees,
    { setup,
      fun () -> start_employees_server(Es, fun employees_req_with_sort/2) end,
      fun (_) ->  stop_employees_server() end,
      make_tests(fun employees_client/1, employees_client_no_sort_test_specs())
    }.
-endif.

-ifdef(test_employees_client_with_sort_dosort).
employees_client_with_sort_dosort_test_() ->
    Es = ?Employees,
    { setup,
      fun () -> start_employees_server(Es, fun employees_req_with_sort/2) end,
      fun (_) ->  stop_employees_server() end,
      make_tests(fun employees_client/1, 
	[
	 { sort, [{sort}], {ok, void} },
	 { sorted_dump, [{dump}], { ok, ?SortedEmployees } }
	])
    }.
-endif.

employees_client_mutate_test_specs() ->
    Es = ?Employees,
    [ { dump0, [{dump}], {ok, Es} },
      { delete_last, [{delete, jane}], {ok, void } },
      { dump1, [{dump}],
       {ok, [ ?Tom, ?Joan, ?Bill, ?John, ?Sue, ?Alice, ?Harry, ?Larry, ?Erwin] }
      },
      { delete_intermediate, [{delete, sue}], { ok, void } },
      { dump2, [{dump}],
       {ok, [ ?Tom, ?Joan, ?Bill, ?John, ?Alice, ?Harry, ?Larry, ?Erwin] }
      },
      { read_intermediate, [{read, alice}], { ok, ?Alice } },
      { read_first, [{read, tom}], { ok, ?Tom } },
      { upsert_first, [{upsert, ?Tom1}], { ok, void } },
      { dump3, [{dump}],
       {ok, [ ?Tom1, ?Joan, ?Bill, ?John, ?Alice, ?Harry, ?Larry, ?Erwin] }
      },
      { upsert_end1, [{upsert, ?Joe1}], { ok, void } },
      { upsert_end2, [{upsert, ?Jane1}], { ok, void } },
      { dump4, [{dump}],
       {ok, [ ?Tom1, ?Joan, ?Bill, ?John, ?Alice, ?Harry, ?Larry, ?Erwin,
	      ?Joe1, ?Jane1] }
      },
      { find_age, [ {find, [employee_has_age(44)]} ], { ok, [?Tom1] } }
    ].
    
-ifdef(test_employees_client_no_sort_mutate).
employees_client_no_sort_mutate_test_() ->
    Es = ?Employees,
    { setup,
      fun () -> start_employees_server(Es, fun employees_req/2) end,
      fun (_) ->  stop_employees_server() end,
      make_tests(fun employees_client/1, employees_client_mutate_test_specs())
    }.
-endif.

-ifdef(test_employees_client_with_sort_mutate).
employees_client_with_sort_mutate_test_() ->
    Es = ?Employees,
    { setup,
      fun () -> start_employees_server(Es, fun employees_req_with_sort/2) end,
      fun (_) ->  stop_employees_server() end,
      make_tests(fun employees_client/1, 
		 employees_client_mutate_test_specs()
		 ++ [ { sort, [{sort}], {ok, void} },
		      { dump5, [{dump}], 
			{ok, [ ?Alice,  ?Bill, ?Erwin, ?Harry, ?Jane1, ?Joan, 
			       ?Joe1, ?John, ?Larry, ?Tom1
			     ] 
			}
		      }
		    ])
    }.
-endif.

-ifdef(test_employees_client_hot_reload).
employees_client_hot_reload_test_() ->
    Es = ?Employees,
    Ignore = fun ignore_err_message/1,
    { setup,
      fun () -> start_employees_server(Es, fun employees_req_with_sort/2) end,
      fun (_) ->  stop_employees_server() end,
      make_tests(fun employees_client/1, 
		 [ { first_sort, [{sort}], {ok, void} },
		   { dump1, [{dump}], {ok, ?SortedEmployees} },
		   { new_fn1, [{new_fn, fun employees_req/2}], {ok, void} },
		   { sort_err, [{sort}], {err}, Ignore },
		   { dump2, [{dump}], {ok, ?SortedEmployees} },
		   { new_fn1, [{new_fn, fun employees_req_with_sort/2}], 
		     {ok, void} 
		   }
		 ])
    }.
-endif.

