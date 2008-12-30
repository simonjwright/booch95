--  Copyright 1998 Pat Rogers.
--  Copyright 1998-2002 Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  $Id$

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;
with BC.Support.Unmanaged_Storage;

with System.Storage_Elements;


procedure Unmanaged_Storage is

   use type System.Storage_Elements.Storage_Count;

   package Unmanaged_Storage renames BC.Support.Unmanaged_Storage;

   type T is array (Integer range <>) of Character;

   --  T_Overhead : constant := 2 * (System.Word_Size / System.Storage_Unit);
   --  to include dope for lower/upper bounds, since T is unconstrained

   Unmanaged_Pool : Unmanaged_Storage.Pool;

   type U_P is access T;
   for U_P'Storage_Pool use Unmanaged_Pool;

   procedure Delete is new Ada.Unchecked_Deallocation (T, U_P);

   U_Ptr : U_P;

   Trying : Integer;

begin

   Ada.Text_IO.Put_Line ("trying unmanaged store:");
   for I in 0 .. 4097 loop
      Trying := I;
      U_Ptr := new T (1 .. Trying);
      Delete (U_Ptr);
   end loop;
   Ada.Text_IO.Put_Line (".. done all 4097 allocations.");

exception
   when BC.Storage_Error =>
      Ada.Text_IO.Put ("exception BC.Storage_Error raised at size");
      Ada.Integer_Text_IO.Put (Trying);
      Ada.Text_IO.New_Line;
end Unmanaged_Storage;
