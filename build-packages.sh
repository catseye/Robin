#!/bin/sh

echo "Building Robin convenience packages..."

mkdir -p pkg

cat stdlib/literal.robin stdlib/env.robin stdlib/list.robin stdlib/bind.robin \
    stdlib/let.robin stdlib/choose.robin \
    stdlib/bind-args.robin > pkg/small.robin

cat stdlib/empty-p.robin stdlib/map.robin stdlib/fold.robin stdlib/reverse.robin \
    stdlib/filter.robin stdlib/find.robin stdlib/append.robin stdlib/elem-p.robin \
    stdlib/length.robin stdlib/index.robin stdlib/take-while.robin stdlib/drop-while.robin \
    stdlib/first.robin stdlib/rest.robin stdlib/last.robin stdlib/prefix-p.robin \
    stdlib/flatten.robin stdlib/lookup.robin stdlib/extend.robin stdlib/delete.robin > pkg/list.robin

cat stdlib/fun.robin > pkg/fun.robin

cat stdlib/abs.robin stdlib/add.robin stdlib/cmp.robin \
    stdlib/multiply.robin stdlib/divide.robin \
    stdlib/remainder.robin > pkg/arith.robin

cat stdlib/boolean-p.robin \
    stdlib/not.robin stdlib/and.robin stdlib/or.robin \
    stdlib/xor.robin > pkg/boolean.robin

cat stdlib/env-p.robin stdlib/export.robin stdlib/sandbox.robin \
    stdlib/unbind.robin stdlib/unshadow.robin > pkg/env.robin

cat stdlib/itoa.robin > pkg/misc.robin

cat pkg/small.robin \
    pkg/fun.robin \
    pkg/boolean.robin \
    pkg/arith.robin \
    pkg/list.robin \
    pkg/env.robin \
    pkg/misc.robin \
    > pkg/stdlib-no-builtins.robin

cat pkg/boolean.robin \
    pkg/arith.robin \
    pkg/list.robin \
    pkg/env.robin \
    pkg/misc.robin \
    > pkg/stdlib.robin
