The following functionalities are used to test Robin Expressions.

    -> Functionality "Evaluate core Robin Expression" is implemented by shell command
    -> "bin/robin --no-builtins eval %(test-body-file)"

    -> Functionality "Evaluate Robin Expression (with literal)" is implemented by shell command
    -> "bin/robin --no-builtins stdlib/literal.robin eval %(test-body-file)"

    -> Functionality "Evaluate Robin Expression (with literal and bind)" is implemented by shell command
    -> "bin/robin --no-builtins stdlib/literal.robin stdlib/bind.robin eval %(test-body-file)"

    -> Functionality "Evaluate Robin Expression (with literal and bind and list)" is implemented by shell command
    -> "bin/robin --no-builtins stdlib/literal.robin stdlib/bind.robin stdlib/list.robin eval %(test-body-file)"

    -> Functionality "Evaluate Robin Expression (with Small)" is implemented by shell command
    -> "bin/robin --no-builtins pkg/small.robin eval %(test-body-file)"

    -> Functionality "Evaluate Robin Expression (with List)" is implemented by shell command
    -> "bin/robin --no-builtins pkg/small.robin pkg/list.robin eval %(test-body-file)"

    -> Functionality "Evaluate Robin Expression (with Env)" is implemented by shell command
    -> "bin/robin --no-builtins pkg/small.robin pkg/list.robin pkg/env.robin eval %(test-body-file)"

    -> Functionality "Evaluate Robin Expression (with Boolean)" is implemented by shell command
    -> "bin/robin --no-builtins pkg/small.robin pkg/boolean.robin eval %(test-body-file)"

    -> Functionality "Evaluate Robin Expression (with Arith)" is implemented by shell command
    -> "bin/robin --no-builtins pkg/small.robin pkg/arith.robin eval %(test-body-file)"

    -> Functionality "Evaluate Robin Expression (with Stdlib)" is implemented by shell command
    -> "bin/robin --no-builtins pkg/stdlib.robin eval %(test-body-file)"

The following functionalities are used to test Robin Toplevel programs.

    -> Functionality "Execute core Robin Toplevel Program" is implemented by shell command
    -> "bin/robin --no-builtins %(test-body-file)"

    -> Functionality "Execute Robin Toplevel Program (with Small)" is implemented by shell command
    -> "bin/robin --no-builtins pkg/small.robin %(test-body-file)"
