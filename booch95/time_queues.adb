-- Copyright (C) 1998 Simon Wright.
-- All Rights Reserved.
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

-- $Id$

-- The  purpose of  this test  procedure is  to check  out  the relative --
-- performance  of the  various flavours  of Queue,  in case  there's an --
-- efficiency problem.                                                   --

with Ada.Calendar;
with Ada.Text_Io;
with Queues_For_Timing;

procedure Time_Queues is

  B : Queues_For_Timing.B.Bounded_Queue;
  D : Queues_For_Timing.D.Dynamic_Queue;
  U : Queues_For_Timing.U.Unbounded_Queue;

  procedure Iterate is
    Total : Integer;
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
    Ada.Text_Io.Put_Line
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
    Ada.Text_Io.Put_Line
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
    Ada.Text_Io.Put_Line
       (".. unbounded queue took"
        & Duration'Image (Taken)
        & " sec, sum"
        & Integer'Image (Total));
  end Iterate;

  procedure Time (N : Integer) is
  begin
    Ada.Text_Io.Put_Line
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
    Ada.Text_Io.Put_Line ("oops");
end Time_Queues;
