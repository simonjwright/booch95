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

--  The purpose of this test procedure is to check out the relative
--  performance of the various flavours of Queue, in case there's an
--  efficiency problem.

with Ada.Calendar;
with Ada.Text_IO;
with Queues_For_Timing;

procedure Time_Queues is

   B : Queues_For_Timing.B.Queue;
   D : Queues_For_Timing.D.Queue;
   U : Queues_For_Timing.U.Queue;

   procedure Iterate;
   procedure Iterate is
      Total : Integer;
      procedure Apply (Elem : Integer; Ok : out Boolean);
      procedure Apply (Elem : Integer; Ok : out Boolean) is
      begin
         Total := Total + Elem;
         Ok := True;
      end Apply;
      procedure B_Application is new Queues_For_Timing.C.Visit (Apply);
      procedure D_Application is new Queues_For_Timing.C.Visit (Apply);
      procedure U_Application is new Queues_For_Timing.C.Visit (Apply);
      Start : Ada.Calendar.Time;
      Taken : Duration;
      It : Queues_For_Timing.C.Iterator'Class
        := Queues_For_Timing.C.New_Iterator
        (Queues_For_Timing.C.Container'Class (B));
      use type Ada.Calendar.Time;
   begin
      Total := 0;
      Start := Ada.Calendar.Clock;
      B_Application (It);
      Taken := Ada.Calendar.Clock - Start;
      Ada.Text_IO.Put_Line
        (".. bounded queue took"
         & Duration'Image (Taken)
         & " sec, sum"
         & Integer'Image (Total));
      Total := 0;
      It := Queues_For_Timing.C.New_Iterator
        (Queues_For_Timing.C.Container'Class (D));
      Start := Ada.Calendar.Clock;
      D_Application (It);
      Taken := Ada.Calendar.Clock - Start;
      Ada.Text_IO.Put_Line
        (".. dynamic queue took"
         & Duration'Image (Taken)
         & " sec, sum"
         & Integer'Image (Total));
      Total := 0;
      It := Queues_For_Timing.C.New_Iterator
        (Queues_For_Timing.C.Container'Class (U));
      Start := Ada.Calendar.Clock;
      U_Application (It);
      Taken := Ada.Calendar.Clock - Start;
      Ada.Text_IO.Put_Line
        (".. unbounded queue took"
         & Duration'Image (Taken)
         & " sec, sum"
         & Integer'Image (Total));
   end Iterate;

   procedure Time (N : Integer);
   procedure Time (N : Integer) is
   begin
      Ada.Text_IO.Put_Line
        ("timing iteration over containers of length" & Integer'Image (N));
      Queues_For_Timing.B.Clear (B);
      Queues_For_Timing.D.Clear (D);
      Queues_For_Timing.D.Preallocate (D, 1024);
      Queues_For_Timing.U.Clear (U);
      for I in 1 .. N loop
         Queues_For_Timing.B.Append (B, I);
         Queues_For_Timing.D.Append (D, I);
         Queues_For_Timing.U.Append (U, I);
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
exception
   when others =>
      Ada.Text_IO.Put_Line ("oops");
end Time_Queues;
