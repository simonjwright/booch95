--  Copyright 1998-2003 Simon Wright <simon@pushface.org>

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

with Ada.Unchecked_Deallocation;

package body BC.Support.Auto_Pointers is


   procedure Delete is new Ada.Unchecked_Deallocation (T, P);


   function Create (With_Value : T) return Pointer is
      Result : Pointer;
   begin
      Result.The_Owner.The_Owned_P :=
        new Owned_P'(The_Owner => Result.The_Owner'Unchecked_Access,
                     The_P => new T'(With_Value));
      return Result;
   end Create;


   function Value (Ptr : Pointer) return T is
   begin
      return Ptr.The_Owner.The_Owned_P.The_P.all;
   end Value;


   procedure Set (Ptr : in out Pointer; To : T) is
   begin
      if Ptr.The_Owner.The_Owned_P = null then
         Ptr.The_Owner.The_Owned_P :=
           new Owned_P'(The_Owner => Ptr.The_Owner'Unchecked_Access,
                        The_P => new T'(To));
      else
         Delete (Ptr.The_Owner.The_Owned_P.The_P);
         Ptr.The_Owner.The_Owned_P.The_P := new T'(To);
      end if;
   end Set;


   function Accessor (Ptr : Pointer) return P is
   begin
      if Ptr.The_Owner.The_Owned_P = null then
         return null;
      else
         return Ptr.The_Owner.The_Owned_P.The_P;
      end if;
   end Accessor;


   procedure Adjust (Obj : in out Pointer) is
   begin
      if Obj.The_Owner.The_Owned_P /= null then
         Obj.The_Owner.The_Owned_P.The_Owner := null;
         Obj.The_Owner.The_Owned_P.The_Owner := Obj.The_Owner'Unchecked_Access;
      end if;
   end Adjust;


   procedure Delete is new Ada.Unchecked_Deallocation (Owned_P, Owned_P_P);

   procedure Finalize (Obj : in out Pointer) is
   begin
      if Obj.The_Owner.The_Owned_P /= null then
         Delete (Obj.The_Owner.The_Owned_P.The_P);
         Delete (Obj.The_Owner.The_Owned_P);
      end if;
   end Finalize;


end BC.Support.Auto_Pointers;
