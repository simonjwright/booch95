-- Copyright (C) 1994-2001 Grady Booch, David Weller and Simon Wright.
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

with Ada.Exceptions;
with Ada.Text_IO;
with BC;
with Ordered_Queue_Test_Support;

procedure Ordered_Queue_Test is
  use Ada.Text_IO;
  use Ordered_Queue_Test_Support;
  use Containers;
  use Queues;
  use QB;
  use QD;
  use QU;

  procedure Process (C : Character; OK : out Boolean) is
  begin
    Put_Line ("Item: " & C);
    Ok := True;
  end Process;

  procedure Assertion (Cond : Boolean; Message : String) is
  begin
    if not Cond then
      Put_Line (Message);
    end if;
  end Assertion;

  procedure Test_Active_Iterator (L : Container'Class) is
    Iter : Iterator'Class := New_Iterator (L);
    Success : Boolean;
    Temp : Character;
  begin
    while not Is_Done (Iter) loop
      Temp := Current_Item (Iter);
      Process (Temp, Success);
      Next (Iter);
    end loop;
  end Test_Active_Iterator;

  procedure Test_Primitive (Q1, Q2 : in out Abstract_Ordered_Queue'Class) is
  begin
    Assertion (Is_Empty (Q1), "** P01: Queue is not initially empty");
    Assertion (Length (Q1) = 0, "** P02: Queue length is not initially zero");
    Append(Q1, '3');
    Append(Q1, '2');
    Append(Q1, '1');
    Assertion (not (Is_Empty (Q1)), "** P03: Queue is empty");
    Assertion ((Length (Q1) = 3), "** P04: Queue length is not correct");
    Assertion ((Front (Q1) = '1'), "** P05: Queue front is not correct");
    Clear(Q1);
    Assertion (Is_Empty (Q1), "** P06: Queue is not empty");
    Assertion ((Length (Q1) = 0), "** P07: Queue length is not zero");
    Append(Q1, '6');
    Append(Q1, '5');
    Append(Q1, '4');
    Assertion (not (Is_Empty (Q1)), "** P08: Queue is empty");
    Assertion ((Length (Q1) = 3), "** P09: Queue length is not correct");
    Assertion ((Front (Q1) = '4'), "** P10: Queue front is not correct");
    Pop(Q1);
    Pop(Q1);
    Assertion (not (Is_Empty (Q1)), "** P11: Queue is empty");
    Assertion ((Length (Q1) = 1), "** P12: Queue length is not correct");
    Assertion ((Front (Q1) = '6'), "** P13: Queue front is not correct");
    Pop(Q1);
    Assertion (Is_Empty (Q1), "** P14: Queue is not empty");
    Assertion ((Length (Q1) = 0), "** P15: Queue length is not zero");
    Append(Q1, '9');
    Append(Q1, '8');
    Append(Q1, '7');
    Pop(Q1);
    Pop(Q1);
    Assertion (not (Is_Empty (Q1)), "** P16: Queue is empty");
    Assertion ((Length (Q1) = 1), "** P17: Queue length is not correct");
    Assertion ((Front (Q1) = '9'), "** P18: Queue front is not correct");
    Q2:= Q1;
    Assertion (not (Is_Empty (Q1)), "** P19: Queue is empty");
    Assertion ((Length (Q1) = 1), "** P20: Queue length is not correct");
    Assertion ((Front (Q1) = '9'), "** P21: Queue front is not correct");
    Assertion (not (Is_Empty (Q2)), "** P22: Queue is empty");
    Assertion ((Length (Q2) = 1), "** P23: Queue length is not correct");
    Assertion ((Front (Q2) = '9'), "** P24: Queue front is not correct");
    Assertion ((Q1 = Q2), "** P25: Queues are not equal");
    Clear(Q2);
    Assertion (not (Is_Empty (Q1)), "** P26: Queue is empty");
    Assertion ((Length (Q1) = 1), "** P27: Queue length is not correct");
    Assertion ((Front (Q1) = '9'), "** P28: Queue front is not correct");
    Assertion (Is_Empty (Q2), "** P29: Queue is not empty");
    Assertion ((Length (Q2) = 0), "** P30: Queue length is not correct");
    Assertion ((Q1 /= Q2), "** P31: Queues not equal");
    Append(Q2, '2');
    Append(Q2, '1');
    Append(Q2, '3');
    Append(Q2, '4');
    Assertion (Location (Q2, '1') = 1,
               "** P32: Queue location is not correct");
    Assertion (Location (Q2, '2') = 2,
               "** P33: Queue location is not correct");
    Assertion (Location (Q2, '4') = 4,
               "** P34: Queue location is not correct");
    Remove(Q2, 1);
    Remove(Q2, 2);
    Remove(Q2, 2);
    Assertion ((Length (Q2) = 1), "** P35: Queue length is not correct");
    Assertion ((Front (Q2) = '2'), "** P36: Queue front is not correct");
    Remove(Q2, 1);
    Assertion ((Length (Q2) = 0), "** P37: Queue length is not correct");
    Append (Q1, 'z');
  end Test_Primitive;

  procedure Test_Passive_Iterator (Q : Container'Class) is
    procedure Iterate is new Visit (Apply => Process);
    Iter : Iterator'Class := New_Iterator (Q);
  begin
    Iterate (Using => Iter);
  end Test_Passive_Iterator;

  procedure Test_Iterator_Deletion (Q : in out Abstract_Ordered_Queue'Class) is
    Iter : Iterator'Class := New_Iterator (Q);
    Delete : Boolean;
  begin
    Clear (Q);
    Append (Q, '6');
    Append (Q, '5');
    Append (Q, '4');
    Append (Q, '3');
    Append (Q, '2');
    Append (Q, '1');
    Delete := False;
    Reset (Iter);
    while not Is_Done (Iter) loop
      if Delete then
        Delete_Item_At (Iter);
        Delete := False;
      else
        Next (Iter);
        Delete := True;
      end if;
    end loop;
    begin
      Delete_Item_At (Iter);
      Assertion (False, "** I01: Deletion succeeded");
    exception
      when BC.Not_Found => null;
      when others =>
        Assertion (False, "** I02: Unexpected exception");
    end;
    Assertion (Length (Q) = 3, "** I03: Queue length is not correct");
    Assertion (Front (Q) = '1', "** I04: Queue item is not correct");
    Pop (Q);
    Assertion (Front (Q) = '3', "** I05: Queue item is not correct");
    Pop (Q);
    Assertion (Front (Q) = '5', "** I06: Queue item is not correct");
    Pop (Q);
    Assertion (Length (Q) = 0, "** I07: Queue length is not zero");
  end Test_Iterator_Deletion;

  Queue_B_P1, Queue_B_P2 : QB.Queue;
  Queue_D_P1, Queue_D_P2 : QD.Queue;
  Queue_U_P1, Queue_U_P2 : QU.Queue;

begin
  Put_Line ("Starting ordered queue tests");

  Put_Line ("...Bounded Ordered Queue");
  Test_Primitive (Queue_B_P1, Queue_B_P2);

  Put_Line ("...Dynamic Ordered Queue");
  QD.Preallocate (Queue_D_P1, 50);
  Test_Primitive (Queue_D_P1, Queue_D_P2);

  Put_Line ("...Unbounded Ordered Queue");
  Test_Primitive (Queue_U_P1, Queue_U_P2);

  Put_Line ("... Ordered Queue Active Iterator");
  Put_Line ("   Bounded:");
  Test_Active_Iterator (Queue_B_P1);
  Put_Line ("   Dynamic:");
  Test_Active_Iterator (Queue_D_P1);
  Put_Line ("   Ordered:");
  Test_Active_Iterator (Queue_U_P1);

  Put_Line ("... Ordered Queue Passive Iterator");
  Put_Line ("   Bounded:");
  Test_Passive_Iterator (Queue_B_P1);
  Put_Line ("   Dynamic:");
  Test_Passive_Iterator (Queue_D_P1);
  Put_Line ("   Unbounded:");
  Test_Passive_Iterator (Queue_U_P1);

  Assertion ((Front (Queue_B_P1) = '9'), "** M01: Queue front is not correct");
  Assertion ((Length (Queue_B_P2) = 0), "** M02: Queue length is not correct");
  Assertion ((Front (Queue_D_P1) = '9'), "** M05: Queue front is not correct");
  Assertion ((Length (Queue_D_P2) = 0), "** M06: Queue length is not correct");
  Assertion ((Front (Queue_U_P1) = '9'), "** M09: Queue front is not correct");
  Assertion ((Length (Queue_U_P2) = 0), "** M10: Queue length is not correct");

  Assertion (Available (Queue_B_P1) = 98,
          "** M13: Available space not correct");
  Assertion (Available (Queue_B_P2) = 100,
          "** M14: Available space not correct");

  Put_Line ("...Ordered Queue Iterator Deletion");
  Put_Line ("   Bounded:");
  Test_Iterator_Deletion (Queue_B_P1);
  Put_Line ("   Dynamic:");
  Test_Iterator_Deletion (Queue_D_P1);
  Put_Line ("   Unbounded:");
  Test_Iterator_Deletion (Queue_U_P1);

  Put_Line ("Completed ordered queue tests");

exception
  when E : others =>
    Put_Line ("                                   EXCEPTION "
              & Ada.Exceptions.Exception_Name (E)
              & " OCCURRED.");
end Ordered_Queue_Test;
