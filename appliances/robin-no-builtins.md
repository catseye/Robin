    -> Functionality "Execute core Robin Program" is implemented by shell command
    -> "bin/robin --no-builtins %(test-body-file)"

    -> Functionality "Execute Robin Program" is implemented by shell command
    -> "bin/robin --no-builtins pkg/small.robin %(test-body-file)"

    -> Functionality "Execute Robin Program (with Small)" is implemented by shell command
    -> "bin/robin --no-builtins pkg/small.robin %(test-body-file)"

    -> Functionality "Execute Robin Program (with List)" is implemented by shell command
    -> "bin/robin --no-builtins pkg/small.robin pkg/list.robin %(test-body-file)"

    -> Functionality "Execute Robin Program (with Env)" is implemented by shell command
    -> "bin/robin --no-builtins pkg/small.robin pkg/list.robin pkg/env.robin %(test-body-file)"

    -> Functionality "Evaluate Robin Expression (with Boolean)" is implemented by shell command
    -> "bin/robin --no-builtins pkg/small.robin pkg/boolean.robin eval %(test-body-file)"

    -> Functionality "Execute Robin Program (with Arith)" is implemented by shell command
    -> "bin/robin --no-builtins pkg/small.robin pkg/arith.robin %(test-body-file)"

    -> Functionality "Execute Robin Program (with List-Arith)" is implemented by shell command
    -> "bin/robin --no-builtins pkg/small.robin pkg/list.robin pkg/arith.robin pkg/list-arith.robin %(test-body-file)"

    -> Functionality "Evaluate Robin Expression (with Stdlib)" is implemented by shell command
    -> "bin/robin --no-builtins pkg/stdlib-no-builtins.robin eval %(test-body-file)"
