    -> Functionality "Interpret core Robin Program" is implemented by shell command
    -> "bin/robin --no-builtins %(test-body-file)"

    -> Functionality "Interpret Robin Program" is implemented by shell command
    -> "bin/robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with Small)" is implemented by shell command
    -> "bin/robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with Fun)" is implemented by shell command
    -> "bin/robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with List)" is implemented by shell command
    -> "bin/robin pkg/list.robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with Boolean)" is implemented by shell command
    -> "bin/robin pkg/boolean.robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with Env)" is implemented by shell command
    -> "bin/robin pkg/list.robin pkg/env.robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with Arith)" is implemented by shell command
    -> "bin/robin pkg/arith.robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with List-Arith)" is implemented by shell command
    -> "bin/robin pkg/list.robin pkg/arith.robin pkg/list-arith.robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with Stdlib)" is implemented by shell command
    -> "bin/robin pkg/stdlib.robin %(test-body-file)"