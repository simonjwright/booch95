--  Copyright (C) 1994 Grady Booch.
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

--  $Id$

--  The Sedgewick algorithm is from "Algorithms", Robert Sedgewick,
--  Addison-Wesley 1983, with adjustments to obviate Constraint_Error.

procedure BC.Containers.Quicksort (C : in out Container) is

   procedure Booch (C : in out Containers.Container'Class;
                    L, R : Natural);
   procedure Sedgewick (C : in out Containers.Container'Class;
                        L, R : Natural);

   procedure Sedgewick (C : in out Containers.Container'Class;
                        L, R : Natural) is
      V : Item;
      I : Natural;
      J : Positive;
      procedure Swap (A, B : Positive);
      pragma Inline (Swap);
      procedure Swap (A, B : Positive) is
         Temp : constant Item := Item_At (C, A).all;
      begin
         Item_At (C, A).all := Item_At (C, B).all;
         Item_At (C, B).all := Temp;
      end Swap;
   begin
      if R > L then
         V := Item_At (C, R).all;
         I := L - 1;
         J := R;
         loop
            loop
               I := I + 1;
               exit when I = R or else V < Item_At (C, I).all;
            end loop;
            loop
               J := J - 1;
               exit when J = L
                 or else Item_At (C, J).all = V
                 or else Item_At (C, J).all < V;
            end loop;
            Swap (I, J);
            exit when J <= I;
         end loop;
         Swap (J, I);
         Swap (I, R);
         Sedgewick (C, L, I - 1);
         Sedgewick (C, I + 1, R);
      end if;
   end Sedgewick;

   procedure Booch (C : in out Containers.Container'Class;
                    L, R : Natural) is
      procedure Swap (A, B : Positive);
      pragma Inline (Swap);
      procedure Swap (A, B : Positive) is
         Temp : constant Item := Item_At (C, A).all;
      begin
         Item_At (C, A).all := Item_At (C, B).all;
         Item_At (C, B).all := Temp;
      end Swap;
   begin
      if R > L + 1 then
         declare
            M : constant Positive := (L + R) / 2;
            Pivot : constant Item := Item_At (C, M).all;
            T : Positive := R;
            B : Positive := L + 1;
            Flag : Boolean := True;
         begin
            --  park the pivot item at the left
            Swap (M, L);
            --  this loop I do _not_ understand
            while B <= T and then Flag loop
               if Pivot < Item_At (C, B).all then
                  Flag := False;
                  while T >= B and then not Flag loop
                     if Item_At (C, T).all < Pivot then
                        Swap (B, T);
                        Flag := True;
                     end if;
                     T := T - 1;
                  end loop;
               end if;
               if Flag then
                  B := B + 1;
               else
                  T := B - 1;
               end if;
            end loop;
            --  put the pivot item in its final position
            Swap (L, T);
            --  sort the left part, if it's longer than one element
            if L < T - 1 then
               Booch (C, L, T - 1);
            end if;
            --  sort the right part, if it's longer than one element
            if T + 1 < R then
               Booch (C, T + 1, R);
            end if;
         end;
      elsif R > L and then Item_At (C, R).all < Item_At (C, L).all then
         --  length is 2, in the wrong order
         Swap (L, R);
      end if;
   end Booch;

begin
   Booch (C, 1, Length (C));
exception
   when Should_Have_Been_Overridden => raise Container_Error;
end BC.Containers.Quicksort;

