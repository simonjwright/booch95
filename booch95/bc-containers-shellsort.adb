--  Copyright (C) 2001 Simon Wright.
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

--  Algorithm from "Algorithms", Robert Sedgewick, Addison-Wesley 1983

procedure BC.Containers.Shellsort (C : in out Container) is
   procedure Sort (C : in out Containers.Container'Class);
   pragma Inline (Sort);
   Len : constant Natural := Length (C);
   procedure Sort (C : in out Containers.Container'Class) is
      H : Positive := 1;
      J : Positive;
      V : Item;
   begin
      loop
         H := 3 * H + 1;
         exit when H > Len;
      end loop;
      loop
         H := H / 3;
         for I in H + 1 .. Len loop
            V := Item_At (C, I).all;
            J := I;
            while V < Item_At (C, J - H).all loop
               Item_At (C, J).all := Item_At (C, J - H).all;
               J := J - H;
               exit when J <= H;
            end loop;
            Item_At (C, J).all := V;
         end loop;
         exit when H = 1;
      end loop;
   end Sort;
begin
   Sort (C);
exception
   when Should_Have_Been_Overridden => raise Sort_Error;
end BC.Containers.Shellsort;
