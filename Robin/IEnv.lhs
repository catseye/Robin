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

> data IEnv t = IEnv (t -> IO t) ThreadId (Chan t)

> stop expr =
>     error ("uncaught exception: " ++ show expr)

> newIEnv eh tid chan =
>     IEnv eh tid chan

> getExceptionHandler (IEnv handler _ _) = handler
> setExceptionHandler handler (IEnv _ tid chan) =
>     (IEnv handler tid chan)

> getThreadId (IEnv _ tid _) = tid
> getChannel (IEnv _ _ chan) = chan
