--  Copyright (C) 1998,2001 Pat Rogers and Simon Wright.
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

package body BC.Support.Unmanaged_Storage is

   type Default_Access_Type is access Integer;  -- arbitrary designated subtype

   --  In the subprograms below, we would use just
   --  Default_Access_Type'Storage_Pool but that GNAT 3.11b has an error in
   --  this area. Thanks to Gene Ouye <geneo@rational.com> for the idea.

   procedure Allocate (The_Pool : in out Pool;
                       Storage_Address : out System.Address;
                       Size_In_Storage_Elements : SSE.Storage_Count;
                       Alignment : SSE.Storage_Count) is
   begin
      SSP.Allocate (SSP.Root_Storage_Pool'Class
                    (Default_Access_Type'Storage_Pool),
                    Storage_Address,
                    Size_In_Storage_Elements,
                    Alignment);
   end Allocate;


   procedure Deallocate (The_Pool : in out Pool;
                         Storage_Address : System.Address;
                         Size_In_Storage_Elements : SSE.Storage_Count;
                         Alignment : SSE.Storage_Count) is
   begin
      SSP.Deallocate (SSP.Root_Storage_Pool'Class
                      (Default_Access_Type'Storage_Pool),
                      Storage_Address,
                      Size_In_Storage_Elements,
                      Alignment);
   end Deallocate;


   function Storage_Size (This : Pool) return SSE.Storage_Count is
   begin
      return SSP.Storage_Size (SSP.Root_Storage_Pool'Class
                               (Default_Access_Type'Storage_Pool));
   end Storage_Size;


end BC.Support.Unmanaged_Storage;
