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

with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Assertions;
with Collection_Test_Support;
with Set_Test_Support;
with Stream_Test_Support;
with Stream_Test_Support.TCB;
with Stream_Test_Support.TCD;
with Stream_Test_Support.TCU;

procedure Stream_Test is

   use Ada.Streams.Stream_IO;
   use Ada.Text_IO;
   use Assertions;
   use Collection_Test_Support;
   use Collection_Test_Support.Collections;
   use Set_Test_Support;
   use Set_Test_Support.Sets;
   use Stream_Test_Support;

   procedure Setup (C : in out Abstract_Collection'Class; Using : String);
   procedure Setup (C : in out Abstract_Collection'Class; Using : String) is
   begin
      Clear (C);
      for I in Using'Range loop
         Append (C, Using (I));
      end loop;
   end Setup;

   procedure Setup (S : in out Abstract_Set'Class; Using : String);
   procedure Setup (S : in out Abstract_Set'Class; Using : String) is
   begin
      Clear (S);
      for I in Using'Range loop
         Add (S, Using (I));
      end loop;
   end Setup;

   procedure Image (C : Abstract_Base_Containers.Container'Class);
   procedure Image (C : Abstract_Base_Containers.Container'Class) is
      It : Abstract_Base_Containers.Iterator'Class :=
        Abstract_Base_Containers.New_Iterator (C);
      use Abstract_Base_Containers;
   begin
      Put_Line ("start");
      while not Is_Done (It) loop
         Ada.Text_IO.Put_Line ("  " & Image (Current_Item (It)));
         Next (It);
      end loop;
      Put_Line ("end");
   end Image;

   F : Ada.Streams.Stream_IO.File_Type;

begin

   Put_Line ("Starting Stream tests");

   Create (F, Name => "test.dat");

   declare
      C1, C2 : CD.Collection;
      use CD;
   begin

      Put_Line ("...Dynamic Collections");

      Reset (F, Mode => Out_File);
      Setup (C1, "");
      Setup (C2, "initial");
      Assertion (C1 /= C2, "CD1: Collections are equal");
      Collection'Output (Stream (F), C1);
      Reset (F, Mode => In_File);
      C2 := Collection'Input (Stream (F));
      Assertion (C1 = C2, "CD2: Collections are unequal");

      Reset (F, Mode => Out_File);
      Setup (C1, "second");
      Setup (C2, "SECOND");
      Assertion (C1 /= C2, "CD3: Collections are equal");
      Collection'Output (Stream (F), C1);
      Reset (F, Mode => In_File);
      C2 := Collection'Input (Stream (F));
      Assertion (C1 = C2, "CD4: Collections are unequal");

   exception
      when E : others =>
         Put_Line ("                                   EXCEPTION "
                   & Ada.Exceptions.Exception_Name (E)
                   & " OCCURRED.");
   end;

   declare
      C1, C2 : CU.Collection;
      use CU;
   begin

      Put_Line ("...Unbounded Collections");

      Reset (F, Mode => Out_File);
      Setup (C1, "");
      Setup (C2, "initial");
      Assertion (C1 /= C2, "CU1: Collections are equal");
      Collection'Output (Stream (F), C1);
      Reset (F, Mode => In_File);
      C2 := Collection'Input (Stream (F));
      Assertion (C1 = C2, "CU2: Collections are unequal");

      Reset (F, Mode => Out_File);
      Setup (C1, "second");
      Setup (C2, "SECOND");
      Assertion (C1 /= C2, "CU3: Collections are equal");
      Collection'Output (Stream (F), C1);
      Reset (F, Mode => In_File);
      C2 := Collection'Input (Stream (F));
      Assertion (C1 = C2, "CU4: Collections are unequal");

   exception
      when E : others =>
         Put_Line ("                                   EXCEPTION "
                   & Ada.Exceptions.Exception_Name (E)
                   & " OCCURRED.");
   end;

   declare
      S1, S2 : SU.Set;
      use SU;
   begin

      Put_Line ("...Unbounded Sets");

      Reset (F, Mode => Out_File);
      Setup (S1, "");
      Setup (S2, "initial");
      Assertion (S1 /= S2, "SU1: Sets are equal");
      Set'Output (Stream (F), S1);
      Reset (F, Mode => In_File);
      S2 := Set'Input (Stream (F));
      Assertion (S1 = S2, "SU2: Sets are unequal");

      Reset (F, Mode => Out_File);
      Setup (S1, "second");
      Setup (S2, "SECOND");
      Assertion (S1 /= S2, "SU3: Sets are equal");
      Set'Output (Stream (F), S1);
      Reset (F, Mode => In_File);
      S2 := Set'Input (Stream (F));
      Assertion (S1 = S2, "SU4: Sets are unequal");

   exception
      when E : others =>
         Put_Line ("                                   EXCEPTION "
                   & Ada.Exceptions.Exception_Name (E)
                   & " OCCURRED.");
   end;

   declare
      C1, C2 : ICB.Collection;
      use ICB;
   begin

      Put_Line ("...Bounded complex Collections");

      Reset (F, Mode => Out_File);
      Append (C1, (Of_Kind => I, I => 1234));
      Append (C1, (Of_Kind => C, C => 'Z'));
      Append (C1, (Of_Kind => Stream_Test_Support.F, F => 0.54321));
      Clear (C2);
      Assertion (C1 /= C2, "ICB1: Collections are equal");
      Collection'Output (Stream (F), C1);
      Reset (F, Mode => In_File);
      C2 := Collection'Input (Stream (F));
      Assertion (C1 = C2, "ICB2: Collections are unequal");

   exception
      when E : others =>
         Put_Line ("                                   EXCEPTION "
                   & Ada.Exceptions.Exception_Name (E)
                   & " OCCURRED.");
   end;

   declare
      C1, C2 : ICD.Collection;
      use ICD;
   begin

      Put_Line ("...Dynamic complex Collections");

      Reset (F, Mode => Out_File);
      Append (C1, (Of_Kind => I, I => 1234));
      Append (C1, (Of_Kind => C, C => 'Z'));
      Append (C1, (Of_Kind => Stream_Test_Support.F, F => 0.54321));
      Clear (C2);
      Assertion (C1 /= C2, "ICD1: Collections are equal");
      Collection'Output (Stream (F), C1);
      Reset (F, Mode => In_File);
      C2 := Collection'Input (Stream (F));
      Assertion (C1 = C2, "ICD2: Collections are unequal");

   exception
      when E : others =>
         Put_Line ("                                   EXCEPTION "
                   & Ada.Exceptions.Exception_Name (E)
                   & " OCCURRED.");
   end;

   declare
      C1, C2 : ICU.Collection;
      use ICU;
   begin

      Put_Line ("...Unbounded complex Collections");

      Reset (F, Mode => Out_File);
      Append (C1, (Of_Kind => I, I => 1234));
      Append (C1, (Of_Kind => C, C => 'Z'));
      Append (C1, (Of_Kind => Stream_Test_Support.F, F => 0.54321));
      Clear (C2);
      Assertion (C1 /= C2, "ICU1: Collections are equal");
      Collection'Output (Stream (F), C1);
      Reset (F, Mode => In_File);
      C2 := Collection'Input (Stream (F));
      Assertion (C1 = C2, "ICU2: Collections are unequal");

   exception
      when E : others =>
         Put_Line ("                                   EXCEPTION "
                   & Ada.Exceptions.Exception_Name (E)
                   & " OCCURRED.");
   end;

   declare

      B1, B2, S1, S2 : Base_Class_P;

   begin

      Put_Line ("...Classwide pointers");

      Reset (F, Mode => Out_File);

      B1 := new Brother'(I => 16#aabb#);
      S1 := new Sister'(B => True);

      Base_Class_P'Output (Stream (F), B1);
      Base_Class_P'Output (Stream (F), S1);

      Reset (F, Mode => In_File);

      B2 := Base_Class_P'Input (Stream (F));
      S2 := Base_Class_P'Input (Stream (F));

      Assertion (B2.all = B1.all, "P1: values are unequal");
      Assertion (S2.all = S1.all, "P2: values are unequal");

   exception
      when E : others =>
         Put_Line ("                                   EXCEPTION "
                   & Ada.Exceptions.Exception_Name (E)
                   & " OCCURRED.");
   end;

   declare
      C1, C2 : TCB.Collection;
      use TCB;
   begin

      Put_Line ("...Bounded tagged Collections");

      Reset (F, Mode => Out_File);
      Append (C1, new Brother'(I => 16#aabb#));
      Append (C1, new Sister'(B => True));
      Append (C2, new Brother'(I => 16#5555#));
      Append (C2, new Sister'(B => False));
      Assertion (C1 /= C2, "TCB1: Collections are equal");
      Collection'Output (Stream (F), C1);
      Reset (F, Mode => In_File);
      C2 := Collection'Input (Stream (F));
      Assertion (C1 = C2, "TCB2: Collections are unequal");

   exception
      when E : others =>
         Put_Line ("                                   EXCEPTION "
                   & Ada.Exceptions.Exception_Name (E)
                   & " OCCURRED.");
   end;

   declare
      C1, C2 : TCD.Collection;
      use TCD;
   begin

      Put_Line ("...Dynamic tagged Collections");

      Reset (F, Mode => Out_File);
      Append (C1, new Brother'(I => 16#aabb#));
      Append (C1, new Sister'(B => True));
      Append (C2, new Brother'(I => 16#5555#));
      Append (C2, new Sister'(B => False));
      Assertion (C1 /= C2, "TCD1: Collections are equal");
      Collection'Output (Stream (F), C1);
      Reset (F, Mode => In_File);
      C2 := Collection'Input (Stream (F));
      Image (C1);
      Image (C2);
      Assertion (C1 = C2, "TCD2: Collections are unequal");

   exception
      when E : others =>
         Put_Line ("                                   EXCEPTION "
                   & Ada.Exceptions.Exception_Name (E)
                   & " OCCURRED.");
   end;

   declare
      C1, C2 : TCU.Collection;
      use TCU;
   begin

      Put_Line ("...Unbounded tagged Collections");

      Reset (F, Mode => Out_File);
      Append (C1, new Brother'(I => 16#aabb#));
      Append (C1, new Sister'(B => True));
      Append (C2, new Brother'(I => 16#5555#));
      Append (C2, new Sister'(B => False));
      Assertion (C1 /= C2, "TCU1: Collections are equal");
      Collection'Output (Stream (F), C1);
      Reset (F, Mode => In_File);
      C2 := Collection'Input (Stream (F));
      Assertion (C1 = C2, "TCU2: Collections are unequal");

   exception
      when E : others =>
         Put_Line ("                                   EXCEPTION "
                   & Ada.Exceptions.Exception_Name (E)
                   & " OCCURRED.");
   end;

   Close (F);

   Put_Line ("Completed Stream tests");

   Assertions.Report;

exception
   when E : others =>
      Put_Line ("                                   EXCEPTION "
                & Ada.Exceptions.Exception_Name (E)
                & " OCCURRED.");
end Stream_Test;
