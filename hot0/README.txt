hot code update / dynamic reloading outside otp

reference: http://erlang.2086793.n4.nabble.com/Dynamic-code-reloading-td2087963.html

> c(hot0).
{ok,hot0}
2> P = hot0:start().
<0.163.0>
3> P ! print_version.
hot0 current version is 1
print_version
4> % set version to 2
4> check_process_code(P, hot0).
false
5> c(hot0).
{ok,hot0}
6> check_process_code(P, hot0).
true
7> % the loop is still blocked at version 1, hence check_process_code = true
7> P ! print_version.
hot0 current version is 1
print_version
8> % now the loop has the new code, check_process_code = false
8> check_process_code(P, hot0).
false
9> P ! print_version.
hot0 current version is 2
print_version
10> % set version to 3
10> c(hot0).
{ok,hot0}
11> % set version to 4
11> erlang:monitor(process, P).
#Ref<0.0.0.914>
12> flush().
ok
13> c(hot0).
{ok,hot0}
14> flush().
Shell got {'DOWN',#Ref<0.0.0.914>,process,<0.163.0>,killed}
ok
15> % erlang vm only holds two versions of the code at once, process died because
15> % version 2 was still referenced (waiting a message) while versions 3 and 4
15> % were compiled and loaded
