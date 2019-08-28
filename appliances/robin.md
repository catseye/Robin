    -> Functionality "Execute core Robin Program" is implemented by shell command
    -> "bin/robin --no-builtins %(test-body-file)"

    -> Functionality "Execute Robin Program (with Small)" is implemented by shell command
    -> "bin/robin %(test-body-file)"

    -> Functionality "Execute Robin Program (with List)" is implemented by shell command
    -> "bin/robin pkg/list.robin %(test-body-file)"

    -> Functionality "Evaluate Robin Expression (with Boolean)" is implemented by shell command
    -> "bin/robin pkg/boolean.robin eval %(test-body-file)"

    -> Functionality "Execute Robin Program (with Env)" is implemented by shell command
    -> "bin/robin pkg/list.robin pkg/env.robin %(test-body-file)"

    -> Functionality "Evaluate Robin Expression (with Arith)" is implemented by shell command
    -> "bin/robin pkg/arith.robin eval %(test-body-file)"

    -> Functionality "Evaluate Robin Expression (with Stdlib)" is implemented by shell command
    -> "bin/robin pkg/stdlib.robin eval %(test-body-file)"
