-- Copyright (C) 1998 Simon Wright and Pat Rogers.
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

With Ada.Text_Io;
with Ada.Integer_Text_Io;
with Ada.Unchecked_Deallocation;
with BC.Support.Unmanaged_Storage;

with System.Storage_Elements;


procedure Unmanaged_Storage is

  use type System.Storage_Elements.Storage_Count;

  package Unmanaged_Storage renames BC.Support.Unmanaged_Storage;

  type T is array (Integer range <>) of Character;

  T_Overhead : constant := 2 * (System.Word_Size / System.Storage_Unit);
    -- to include dope for lower/upper bounds, since T is unconstrained

  Unmanaged_Pool : Unmanaged_Storage.Pool;

  type U_P is access T;
  for U_P'Storage_Pool use Unmanaged_Pool;

  procedure Delete is new Ada.Unchecked_Deallocation (T, U_P);

  U_Ptr : U_P;

  Trying : Integer;

begin

  Ada.Text_Io.Put_Line ("trying unmanaged store:");
  for I in 0 .. 4097 loop
    Trying := I;
    U_Ptr := new T(1 .. Trying);
    Delete (U_Ptr);
  end loop;
  Ada.Text_Io.Put_Line (".. done all 4097 allocations.");

exception
  when BC.Storage_Error =>
    Ada.Text_Io.Put ("exception BC.Storage_Error raised at size");
    Ada.Integer_Text_Io.Put (Trying);
    Ada.Text_Io.New_Line;
end Unmanaged_Storage;

