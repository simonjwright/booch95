-- Copyright (C) 1998 Pat Rogers and Simon Wright.
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

package body BC.Support.Unmanaged_Storage is

  type Default_Access_Type is access Integer;  -- arbitrary designated subtype

  Default_Pool : SSP.Root_Storage_Pool'Class
     renames SSP.Root_Storage_Pool'Class( Default_Access_Type'Storage_Pool );
  -- This conversion is necessary to work round a problem in GNAT 3.11b.


  procedure Allocate( The_Pool                 : in out Pool;
                      Storage_Address          :    out System.Address;
                      Size_In_Storage_Elements : in     SSE.Storage_Count;
                      Alignment                : in     SSE.Storage_Count ) is
  begin
    SSP.Allocate( Default_Pool,
                  Storage_Address,
                  Size_In_Storage_Elements,
                  Alignment );
  end Allocate;


  procedure Deallocate( The_Pool                 : in out Pool;
                        Storage_Address          : in     System.Address;
                        Size_In_Storage_Elements : in     SSE.Storage_Count;
                        Alignment                : in     SSE.Storage_Count ) is
  begin
    SSP.Deallocate( Default_Pool,
                    Storage_Address,
                    Size_In_Storage_Elements,
                    Alignment );
  end Deallocate;


  function Storage_Size( This : Pool ) return SSE.Storage_Count is
  begin
    return SSP.Storage_Size( Default_Pool );
  end Storage_Size;


end BC.Support.Unmanaged_Storage;


