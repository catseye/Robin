    -> Functionality "Interpret core Robin Program" is implemented by shell command
    -> "bin/whitecap --no-builtins %(test-body-file)"

    -> Functionality "Interpret Robin Program" is implemented by shell command
    -> "bin/whitecap %(test-body-file)"

    -> Functionality "Interpret Robin Program (with Small)" is implemented by shell command
    -> "bin/whitecap %(test-body-file)"

    -> Functionality "Interpret Robin Program (with Fun)" is implemented by shell command
    -> "bin/whitecap %(test-body-file)"

    -> Functionality "Interpret Robin Program (with List)" is implemented by shell command
    -> "bin/whitecap pkg/list.robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with Boolean)" is implemented by shell command
    -> "bin/whitecap pkg/boolean.robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with Env)" is implemented by shell command
    -> "bin/whitecap pkg/list.robin pkg/env.robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with Arith)" is implemented by shell command
    -> "bin/whitecap pkg/arith.robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with List-Arith)" is implemented by shell command
    -> "bin/whitecap pkg/list.robin pkg/arith.robin pkg/list-arith.robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with Stdlib)" is implemented by shell command
    -> "bin/whitecap pkg/stdlib-for-robini.robin %(test-body-file)"
