--  Copyright (C) 2002 Simon Wright.
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

with Ada.Numerics.Discrete_Random;

package body Configuration_Demo_Support is

   type Special_Integer is mod 2 ** 31;  --  simple to convert to Natural
   Character_Hash : array (Character) of Special_Integer;

   function Hash (S : Ada.Strings.Unbounded.Unbounded_String) return Natural is
      K : Special_Integer := 0;
      N : Special_Integer := 0;
      use Ada.Strings.Unbounded;
   begin

      for M in 1 .. Length (S) loop
         N := Character_Hash (Element (S, M));
         K := K + Special_Integer (M) * N;
      end loop;

      return Natural (K);

   end Hash;

   package Random_Integer is new
     Ada.Numerics.Discrete_Random (Result_Subtype => Special_Integer);

   Generator : Random_Integer.Generator;

begin
   Random_Integer.Reset (Gen => Generator, Initiator => 10009);
   for K in Character_Hash'Range loop
      Character_Hash (K) :=
        Random_Integer.Random (Gen => Generator);
   end loop;
end Configuration_Demo_Support;

