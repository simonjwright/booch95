-- Copyright (C) 1998 Pat Rogers.
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

with System.Storage_Pools;
with System.Storage_Elements;

package BC.Support.Unmanaged_Storage is

  pragma Elaborate_Body;

  package SSE renames System.Storage_Elements;
  package SSP renames System.Storage_Pools;

  type Pool is new SSP.Root_Storage_Pool with private;


  procedure Allocate( The_Pool                 : in out Pool;
                      Storage_Address          :    out System.Address;
                      Size_In_Storage_Elements : in     SSE.Storage_Count;
                      Alignment                : in     SSE.Storage_Count );

  procedure Deallocate( The_Pool                 : in out Pool;
                        Storage_Address          : in     System.Address;
                        Size_In_Storage_Elements : in     SSE.Storage_Count;
                        Alignment                : in     SSE.Storage_Count );

  function Storage_Size( This : Pool ) return SSE.Storage_Count;

private

  type Pool is new SSP.Root_Storage_Pool with null record;

end BC.Support.Unmanaged_Storage;
