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
with BC.Support.Managed_Storage;

with System.Storage_Elements;


procedure Storage is

  use type System.Storage_Elements.Storage_Count;

  package Managed_Storage renames BC.Support.Managed_Storage;

  type T is array (Integer range <>) of Character;

  T_Overhead : constant := 2 * (System.Word_Size / System.Storage_Unit);
    -- to include dope for lower/upper bounds, since T is unconstrained

  Managed_Pool : Managed_Storage.Pool
     (4096 + Managed_Storage.Pool_Overhead (T_Overhead, T'Alignment));

  type M_P is access T;
  for M_P'Storage_Pool use Managed_Pool;

  procedure Delete is new Ada.Unchecked_Deallocation (T, M_P);

  M_Ptr : M_P;

  Trying : Integer;

begin

  Ada.Text_Io.Put_Line ("trying managed store:");
  for I in 0 .. 4097 loop
    Trying := I;
    M_Ptr := new T(1 .. Trying);
    Delete (M_Ptr);
  end loop;
  Ada.Text_Io.Put_Line (".. done.");

exception
  when BC.Storage_Error =>
    Ada.Text_Io.Put ("exception BC.Storage_Error raised at size");
    Ada.Integer_Text_Io.Put (Trying);
    Ada.Text_Io.New_Line;
end Storage;

