Tasks
=====

A task is created with `spawn`. Spawn can pass values to the child. Spawn
returns the child id to the parent.

A child can possibly find its parent's id.

Tasks can possibly send messages to each other using their ids.

A parent can find out when a child has crashed; either as a message or using as
a special construct. If a parent doesn't handle child crashes, the crash may
propagate to the parent instead.

When a task terminates gracefully, its return value may be returned to the
parent. The parent may receive this value; either as a message or using a
special construct.

A parent may wait for a child to terminate.

How do we combine this?


