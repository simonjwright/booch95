--  Copyright (C) 2001 Simon Wright.
--  All Rights Reserved.
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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with BC.Containers.Collections.Bounded;
with BC.Containers.Collections.Dynamic;
with BC.Containers.Collections.Unbounded;
with BC.Support.Managed_Storage;
with BC.Support.Standard_Storage;
with System.Storage_Pools;

package Storage_Timing_Support is

   package Abstract_Containers is new BC.Containers (Integer);
   package Abstract_Collections is new Abstract_Containers.Collections;

   package Bounded_Collections is new Abstract_Collections.Bounded
     (Maximum_Size => 1_000);

   package Dynamic_Collections is new Abstract_Collections.Dynamic
     (Storage => BC.Support.Standard_Storage.Pool,
      Initial_Size => 100);

   Pool : BC.Support.Managed_Storage.Pool (4_000);

   package Managed_Collections is new Abstract_Collections.Unbounded
     (Storage => System.Storage_Pools.Root_Storage_Pool'Class (Pool));

   package Unmanaged_Collections is new Abstract_Collections.Unbounded
     (Storage => BC.Support.Standard_Storage.Pool);

end Storage_Timing_Support;
