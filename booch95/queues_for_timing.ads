-- Copyright (C) 1998 Simon Wright.
-- All Rights Reserved.
--
--      This program is free software; you can redistribute it
--      and/or modify it under the terms of the Ada Community
--      License which comes with this Library.
--
--      This program is distributed in the hope that it will be
--      useful, but WITHOUT ANY WARRANTY; without even the implied
--      warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--      PURPOSE. See the Ada Community License for more details.
--      You should have received a copy of the Ada Community
--      License with this library, in the file named "Ada Community
--      License" or "ACL". If not, contact the author of this library
--      for a copy.
--

-- $Id$

with BC.Containers;
with BC.Containers.Queues;
with BC.Containers.Queues.Bounded;
with BC.Containers.Queues.Dynamic;
with BC.Containers.Queues.Unbounded;
with BC.Support.Managed_Storage;
with BC.Support.Unmanaged_Storage;

package Queues_For_Timing is
  Size : constant := 10_000;
  package C is new BC.Containers (Integer);
  package Q is new C.Queues;
  package B is new Q.Bounded (Size);
  P : BC.Support.Managed_Storage.Pool (10_000);
  package D is new Q.Dynamic (BC.Support.Managed_Storage.Pool, P);
  package U is new Q.Unbounded (BC.Support.Managed_Storage.Pool, P);
--   P : BC.Support.Unmanaged_Storage.Pool;
--   package D is new Q.Dynamic (BC.Support.Unmanaged_Storage.Pool, P);
--   package U is new Q.Unbounded (BC.Support.Unmanaged_Storage.Pool, P);
end Queues_For_Timing;
