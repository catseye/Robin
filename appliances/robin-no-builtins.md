The following functionalities are used to test Robin Expressions.

    -> Functionality "Evaluate core Robin Expression" is implemented by shell command
    -> "bin/robin --no-builtins eval %(test-body-file)"

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
    -> "bin/robin --no-builtins pkg/stdlib-no-builtins.robin eval %(test-body-file)"

The following functionalities are used to test Robin Toplevel programs.

    -> Functionality "Execute core Robin Program" is implemented by shell command
    -> "bin/robin --no-builtins %(test-body-file)"

    -> Functionality "Execute Robin Program (with Small)" is implemented by shell command
    -> "bin/robin --no-builtins pkg/small.robin %(test-body-file)"
