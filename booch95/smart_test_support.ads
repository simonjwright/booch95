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

with Ada.Finalization;
with BC.Smart;

package Smart_Test_Support is

   --  This type is used to test BC.Smart. It's Controlled simply so
   --  that we can see when instances are deleted.
   type T is new Ada.Finalization.Controlled with record
      C : Character;
   end record;
   type P is access T;

   procedure Finalize (The_T : in out T);

   package Smart is new BC.Smart (T => T, P => P);

   function Create (Ch : Character) return Smart.Pointer;
   function Value (P : Smart.Pointer) return Character;

end Smart_Test_Support;
