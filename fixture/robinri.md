    -> Functionality "Interpret core Robin Program" is implemented by shell command
    -> "bin/robinri %(test-body-file)"

    -> Functionality "Interpret Robin Program" is implemented by shell command
    -> "bin/robinri pkg/small.robin pkg/intrinsics-wrappers.robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with Small)" is implemented by shell command
    -> "bin/robinri pkg/small.robin pkg/intrinsics-wrappers.robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with Fun)" is implemented by shell command
    -> "bin/robinri pkg/small.robin pkg/intrinsics-wrappers.robin pkg/fun.robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with List)" is implemented by shell command
    -> "bin/robinri pkg/small.robin pkg/intrinsics-wrappers.robin pkg/fun.robin pkg/list.robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with Env)" is implemented by shell command
    -> "bin/robinri pkg/small.robin pkg/intrinsics-wrappers.robin pkg/fun.robin pkg/list.robin pkg/env.robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with Boolean)" is implemented by shell command
    -> "bin/robinri pkg/small.robin pkg/intrinsics-wrappers.robin pkg/boolean.robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with Arith)" is implemented by shell command
    -> "bin/robinri pkg/small.robin pkg/intrinsics-wrappers.robin pkg/fun.robin pkg/arith.robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with List-Arith)" is implemented by shell command
    -> "bin/robinri pkg/small.robin pkg/intrinsics-wrappers.robin pkg/fun.robin pkg/list.robin pkg/arith.robin pkg/list-arith.robin %(test-body-file)"

    -> Functionality "Interpret Robin Program (with Stdlib)" is implemented by shell command
    -> "bin/robinri pkg/stdlib.robin %(test-body-file)"
