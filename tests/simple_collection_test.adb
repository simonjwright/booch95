--  Copyright 2001-2009 Simon Wright <simon@pushface.org>

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

--  $Revision$
--  $Date$
--  $Author$

with Ada.Exceptions;
with Ada.Text_IO;
with Assertions;
with BC;
with Simple_Collection_Test_Support;

procedure Simple_Collection_Test is

   use Ada.Text_IO;
   use Assertions;
   use Simple_Collection_Test_Support;
   use SC;

   procedure Test_Primitive (C1, C2 : in out Collection);

   procedure Test_Primitive (C1, C2 : in out Collection) is
   begin
      for C in Character'('a') .. Character'('z') loop
         Append (C1, C);
      end loop;
      Clear (C1);
      Assertion (Is_Empty (C1), "** P01: Collection is not initially empty");
      Assertion (Length (C1) = 0,
                 "** P02: Collection Length is not initially zero");
      Insert (C1, '1');
      Insert (C1, '3', 1);
      Insert (C1, '2', 2);
      Assertion (not Is_Empty (C1), "** P03: Collection is empty");
      Assertion (Length (C1) = 3, "** P04: Collection Length is not correct");
      Assertion (First (C1) = '3', "** P05: Collection First is not correct");
      Assertion (Last (C1) = '1', "** P05: Collection Last is not correct");
      Assertion (Item_At (C1, 2) = '2',
                 "** P07: Collection Item is not correct");
      Clear (C1);
      Append (C1, '6');
      Append (C1, '4');
      Append (C1, '5', 1);
      Assertion (not Is_Empty (C1), "** P08: Collection is empty");
      Assertion (Length (C1) = 3, "** P09: Collection Length is not correct");
      Assertion (First (C1) = '6', "** P10: Collection First is not correct");
      Assertion (Last (C1) = '4', "** P11: Collection Last is not correct");
      Remove (C1, 2);
      Remove (C1, 1);
      Assertion (Length (C1) = 1, "** P12: Collection Length is not correct");
      Assertion (First (C1) = '4', "** P13: Collection First is not correct");
      Remove (C1, 1);
      Assertion (Is_Empty (C1), "** P14: Collection is not empty");
      Assertion (Length (C1) = 0, "** P15: Collection Length is not zero");
      Insert (C1, '8');
      Append (C1, '7');
      Insert (C1, '9');
      Remove (C1, 2);
      Remove (C1, 1);
      Assertion (Item_At (C1, 1) = '7',
                 "** P16: Collection Item is not correct");
      Assertion (First (C1) = '7', "** P17: Collection First is not correct");
      Assertion (Last (C1) = '7', "** P18: Collection Last is not correct");
      C2 := C1;
      Assertion (not Is_Empty (C1), "** P19: Collection is empty");
      Assertion (Length (C1) = 1, "** P20: Collection Length is not correct");
      Assertion (First (C1) = '7', "** P21: Collection First is not correct");
      Assertion (not Is_Empty (C2), "** P22: Collection is empty");
      Assertion (Length (C2) = 1, "** P23: Collection Length is not correct");
      Assertion (Last (C2) = '7', "** P24: Collection Last is not correct");
      Assertion (C1 = C2, "** P25: Collections are not equal");
      Clear (C2);
      Assertion (not Is_Empty (C1), "** P26: Collection is empty");
      Assertion (Length (C1) = 1, "** P27: Collection Length is not correct");
      Assertion (Item_At (C1, 1) = '7',
                 "** P28: Collection Item is not correct");
      Assertion (Is_Empty (C2), "** P29: Collection is not empty");
      Assertion (Length (C2) = 0, "** P30: Collection Length is not correct");
      Assertion (C1 /= C2, "** P31: Collections not equal");
      for C in reverse Character'('a') .. Character'('z') loop
         Append (C2, C);
      end loop;
      C1 := C2;
      Assertion (C1 = C2, "** P32: Collections are not equal");
      Assertion (Location (C1, 'g') = 20,
                 "** P33: Collection Location is not correct");
      Replace (C1, 20, 'A');
      Assertion (Item_At (C1, 20) = 'A',
                 "** P34: Collection Item is not correct");
      Replace (C1, 20, 'g');
      Append (C1, 'A');
      Assertion (Item_At (C1, 20) = 'g',
                 "** P35: Collection Item is not correct");
      Clear (C1);
      Clear (C2);
      Insert (C1, '7');
      Insert (C1, 'z');
   end Test_Primitive;

   Collection_1, Collection_2 : Collection;

begin

   Test_Primitive (Collection_1, Collection_2);

end Simple_Collection_Test;
