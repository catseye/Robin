> module Robin.IEnv where

> import Control.Concurrent (ThreadId)
> import Robin.Chan (Chan)

Internal Environments
=====================

This is the evaluation environment for Robin which is entirely
internal; Robin programs cannot see or modify it directly.  Here
we keep things like:

* the continuation which is the current exception handler
* the ThreadId and Chan of the current Pid
* whether tracing is enabled or not

> data IEnv t = IEnv (t -> IO t) ThreadId (Chan t) Bool

> stop expr =
>     error ("uncaught exception: " ++ show expr)

> newIEnv eh tid chan trace =
>     IEnv eh tid chan trace

> getExceptionHandler (IEnv handler _ _ _) = handler
> setExceptionHandler handler (IEnv _ tid chan trace) =
>     (IEnv handler tid chan trace)

> getThreadId (IEnv _ tid _ _) = tid
> getChannel (IEnv _ _ chan _) = chan

> getTrace (IEnv _ _ _ trace) = trace
