--  Copyright 2006 Simon Wright <simon@pushface.org>

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

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with Ada.Finalization;

generic
   type T (<>) is private;
   type P is access T;
package BC.Support.Auto_Pointers is

   pragma Elaborate_Body;

   type Pointer is private;
   --  A Pointer variable encapsulates a T value.

   function Create (With_Value : T) return Pointer;
   pragma Inline (Create);
   --  Returns a new encapsulation.

   function Value (Ptr : Pointer) return T;
   pragma Inline (Value);
   --  Returns the encapsulated value.

   procedure Set (Ptr : in out Pointer; To : T);
   pragma Inline (Set);
   --  Updates the encapsulated value.

   function Accessor (Ptr : Pointer) return P;
   pragma Inline (Accessor);
   --  Designates the encapsulated value.

private

   type Owner;
   type Owner_P is access all Owner;

   type Owned_P is record
      The_Owner : Owner_P;
      The_P : P;
   end record;
   type Owned_P_P is access Owned_P;

   type Owner is record
      The_Owned_P : Owned_P_P;
   end record;

   type Pointer is new Ada.Finalization.Controlled with record
      The_Owner : aliased Owner;
   end record;

   procedure Adjust (Obj : in out Pointer);
   procedure Finalize (Obj : in out Pointer);

end BC.Support.Auto_Pointers;
