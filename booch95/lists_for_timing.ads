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
with BC.Containers.Lists;
with BC.Containers.Lists.Single;
with BC.Containers.Lists.Double;
with BC.Support.Managed_Storage;
with BC.Support.Unmanaged_Storage;
package Lists_For_Timing is
  type Integer_P is access all Integer;
  package C is new BC.Containers (Integer, Integer_P);
  package L is new C.Lists;
  P : BC.Support.Managed_Storage.Pool (10_000);
  package S is new L.Single (BC.Support.Managed_Storage.Pool, P);
  package D is new L.Double (BC.Support.Managed_Storage.Pool, P);
--   P : BC.Support.Unmanaged_Storage.Pool;
end Lists_For_Timing;
