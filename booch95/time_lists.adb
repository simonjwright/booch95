--  Copyright (C) 1998,2000-2001 Simon Wright.
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

--  The purpose of this test procedure is to check out the relative
--  performance of the various flavours of List, in case there's an
--  efficiency problem.

with Ada.Calendar;
with Ada.Text_IO;
with Lists_For_Timing;

procedure Time_Lists is

   S : Lists_For_Timing.S.List;
   D : Lists_For_Timing.D.List;

   procedure Iterate;
   procedure Iterate is
      Total : Integer;
      procedure Apply (Elem : Integer; Ok : out Boolean);
      procedure Apply (Elem : Integer; Ok : out Boolean) is
      begin
         Total := Total + Elem;
         Ok := True;
      end Apply;
      procedure S_Application is new Lists_For_Timing.C.Visit (Apply);
      procedure D_Application is new Lists_For_Timing.C.Visit (Apply);
      Start : Ada.Calendar.Time;
      Taken : Duration;
      It : Lists_For_Timing.C.Iterator'Class
        := Lists_For_Timing.C.New_Iterator
        (Lists_For_Timing.C.Container'Class (S));
      use type Ada.Calendar.Time;
   begin
      Total := 0;
      Start := Ada.Calendar.Clock;
      S_Application (It);
      Taken := Ada.Calendar.Clock - Start;
      Ada.Text_IO.Put_Line
        (".. single list took"
         & Duration'Image (Taken)
         & " sec, sum"
         & Integer'Image (Total));

      Total := 0;
      Start := Ada.Calendar.Clock;
      D_Application (It);
      Taken := Ada.Calendar.Clock - Start;
      Ada.Text_IO.Put_Line
        (".. double list took"
         & Duration'Image (Taken)
         & " sec, sum"
         & Integer'Image (Total));
   end Iterate;

   procedure Time (N : Integer);
   procedure Time (N : Integer) is
   begin
      Ada.Text_IO.Put_Line
        ("timing iteration over containers of length" & Integer'Image (N));
      Lists_For_Timing.S.Clear (S);
      Lists_For_Timing.D.Clear (D);
      for I in 1 .. N loop
         Lists_For_Timing.S.Append (S, I);
         Lists_For_Timing.D.Append (D, I);
      end loop;
      Iterate;
   end Time;

begin
   Time (1);
   Time (2);
   Time (4);
   Time (8);
   Time (16);
   Time (32);
   Time (64);
   Time (128);
   Time (256);
   Time (512);
   Time (1024);
   Time (2048);
   Time (4096);
exception
   when others =>
      Ada.Text_IO.Put_Line ("oops");
end Time_Lists;
