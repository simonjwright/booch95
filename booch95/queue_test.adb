with Text_Io;
with Root_Container;
with Root_Queues;
with Root_Bounded_Queues;
with Root_Dynamic_Queues;
with Root_Unbounded_Queues;
-- with User_Container;
-- with User_Queues;
-- with User_Bounded_Queues;
-- with User_Dynamic_Queues;
-- with User_Unbounded_Queues;
with User_Defined;
with Character_References;
procedure Queue_Test is
   use Text_IO;
   use ROot_Container;
   use Root_Queues;
   use Root_Bounded_Queues;
   use Root_Dynamic_Queues;
   use Root_Unbounded_Queues;
   use Character_References;
--    use User_Container;
--    use User_Queues;
--    use User_Bounded_Queues;
--    use User_Dynamic_Queues;
--    use User_Unbounded_Queues;
   use User_Defined;

   Global_Items : array(1..10) of aliased Chunk;

   procedure Process (C : in Char_Ptr; OK : out Boolean) is
      use Text_IO;
   begin
      Put_Line("Item: " & C.all );
      Ok := True;
   end Process;

   function mod_op is new Modify(Apply=> Process);

   procedure Assertion ( Cond : Boolean; Message : String) is
   begin
      if not Cond then
     Put_Line(Message);
      end if;
   end Assertion;
   pragma Inline(Assertion);

    procedure Test_Active_Iterator (L : access Container'Class) is
       Iter : Iterator(L);
       Success : Boolean;
       Temp : Char_Ptr;
    begin
       while not Is_Done(Iter) loop
	  Temp := Current_Item(Iter);
          Process(Temp, Success);
          Next(Iter);
       end loop;
    end Test_Active_Iterator;

   procedure Test_Primitive(Q1, Q2 : in out Queue'Class) is
   begin
      Assertion(Is_Empty(Q1), "** P01: Queue is not initially empty");
      Assertion(Length(Q1) = 0, "** P02: Queue length is not initially zero");
      Append(Q1, '1');
      Append(Q1, '2');
      Append(Q1, '3');
      Assertion(not (Is_Empty(Q1)), "** P03: Queue is empty");
      Assertion((Length(Q1) = 3), "** P04: Queue length is not correct");
      Assertion((Front(Q1) = '1'), "** P05: Queue front is not correct");
      Clear(Q1);
      Assertion(Is_Empty(Q1), "** P06: Queue is not empty");
      Assertion((Length(Q1) = 0), "** P07: Queue length is not zero");
      Append(Q1, '4');
      Append(Q1, '5');
      Append(Q1, '6');
      Assertion(not (Is_Empty(Q1)), "** P08: Queue is empty");
      Assertion((Length(Q1) = 3), "** P09: Queue length is not correct");
      Assertion((Front(Q1) = '4'), "** P10: Queue front is not correct");
      Pop(Q1);
      Pop(Q1);
      Assertion(not (Is_Empty(Q1)), "** P11: Queue is empty");
      Assertion((Length(Q1) = 1), "** P12: Queue length is not correct");
      Assertion((Front(Q1) = '6'), "** P13: Queue front is not correct");
      Pop(Q1);
      Assertion(Is_Empty(Q1), "** P14: Queue is not empty");
      Assertion((Length(Q1) = 0), "** P15: Queue length is not zero");
      Append(Q1, '7');
      Append(Q1, '8');
      Append(Q1, '9');
      Pop(Q1);
      Pop(Q1);
      Assertion(not (Is_Empty(Q1)), "** P16: Queue is empty");
      Assertion((Length(Q1) = 1), "** P17: Queue length is not correct");
      Assertion((Front(Q1) = '9'), "** P18: Queue front is not correct");
      Q2:= Q1;
      Assertion(not (Is_Empty(Q1)), "** P19: Queue is empty");
      Assertion((Length(Q1) = 1), "** P20: Queue length is not correct");
      Assertion((Front(Q1) = '9'), "** P21: Queue front is not correct");
      Assertion(not (Is_Empty(Q2)), "** P22: Queue is empty");
      Assertion((Length(Q2) = 1), "** P23: Queue length is not correct");
      Assertion((Front(Q2) = '9'), "** P24: Queue front is not correct");
      Assertion((q1 = q2), "** P25: Queues are not equal");
      Clear(Q2);
      Assertion( ( not(Is_Empty(Q1)) ) , "** P26: Queue is empty");
      Assertion(Is_Empty(Q2), "** P29: Queue is not empty");
      Assertion((Length(Q1) = 1), "** P27: Queue length is not correct");
      Assertion((Front(Q1) = '9'), "** P28: Queue front is not correct");
      Assertion((Length(Q2) = 0), "** P30: Queue length is not correct");
      Assertion((q1 /= q2), "** P31: Queues not equal");
      Append(Q2, '1');
      Append(Q2, '2');
      Append(Q2, '3');
      Append(Q2, '4');
      Assertion(Location(Q2, '1') = 1, "** P32: Queue location is not correct");
      Assertion(Location(Q2, '2') = 2, "** P33: Queue location is not correct");
      Assertion(Location(Q2, '4') = 4, "** P34: Queue location is not correct");
      Remove(Q2, 1);
      Remove(Q2, 2);
      Remove(Q2, 2);
      Assertion((Length(Q2) = 1), "** P35: Queue length is not correct");
      Assertion((Front(Q2) = '2'), "** P36: Queue front is not correct");
      Remove(Q2, 1);
      Assertion((Length(Q2) = 0), "** P37: Queue length is not correct");
   end Test_Primitive;

--    procedure Test_User_Defined(Q1, Q2 : in out Queue'Class) is
--    begin
--       Assertion(Is_Empty(Q1), "** P01: Queue is not initially empty");
--       Assertion(Length(Q1) = 0, "** P02: Queue length is not initially zero");
--       Append(Q1, Global_Items(1));
--       Append(Q1, Global_Items(2));
--       Append(Q1, Global_Items(3));
--       Assertion(not (Is_Empty(Q1)), "** P03: Queue is empty");
--       Assertion((Length(Q1) = 3), "** P04: Queue length is not correct");
--       Assertion((Front(Q1) = Global_Items(1)), "** P05: Queue front is not correct");
--       Clear(Q1);
--       Assertion(Is_Empty(Q1), "** P06: Queue is not empty");
--       Assertion((Length(Q1) = 0), "** P07: Queue length is not zero");
--       Append(Q1, Global_Items(4));
--       Append(Q1, Global_Items(5));
--       Append(Q1, Global_Items(6));
--       Assertion(not (Is_Empty(Q1)), "** P08: Queue is empty");
--       Assertion((Length(Q1) = 3), "** P09: Queue length is not correct");
--       Assertion((Front(Q1) = Global_Items(4)), "** P10: Queue front is not correct");
--       Pop(Q1);
--       Pop(Q1);
--       Assertion(not (Is_Empty(Q1)), "** P11: Queue is empty");
--       Assertion((Length(Q1) = 1), "** P12: Queue length is not correct");
--       Assertion((Front(Q1) = Global_Items(6)), "** P13: Queue front is not correct");
--       Pop(Q1);
--       Assertion(Is_Empty(Q1), "** P14: Queue is not empty");
--       Assertion((Length(Q1) = 0), "** P15: Queue length is not zero");
--       Append(Q1, Global_Items(7));
--       Append(Q1, Global_Items(8));
--       Append(Q1, Global_Items(9));
--       Pop(Q1);
--       Pop(Q1);
--       Assertion(not (Is_Empty(Q1)), "** P16: Queue is empty");
--       Assertion((Length(Q1) = 1), "** P17: Queue length is not correct");
--       Assertion((Front(Q1) = Global_Items(9)), "** P18: Queue front is not correct");
--       Q2:= Q1;
--       Assertion(not (Is_Empty(Q1)), "** P19: Queue is empty");
--       Assertion((Length(Q1) = 1), "** P20: Queue length is not correct");
--       Assertion((Front(Q1) = Global_Items(9)), "** P21: Queue front is not correct");
--       Assertion(not (Is_Empty(Q2)), "** P22: Queue is empty");
--       Assertion((Length(Q2) = 1), "** P23: Queue length is not correct");
--       Assertion((Front(Q2) = Global_Items(9)), "** P24: Queue front is not correct");
--       Assertion((q1 = q2), "** P25: Queues are not equal");
--       Clear(Q2);
--       Assertion( ( not(Is_Empty(Q1)) ) , "** P26: Queue is empty");
--       Assertion(Is_Empty(Q2), "** P29: Queue is not empty");
--       Assertion((Length(Q1) = 1), "** P27: Queue length is not correct");
--       Assertion((Front(Q1) = Global_Items(9)), "** P28: Queue front is not correct");
--       Assertion((Length(Q2) = 0), "** P30: Queue length is not correct");
--       Assertion((q1 /= q2), "** P31: Queues not equal");
--        pragma Debug(Put_Line("Pre-Append: Expecting(from q1): 9"));
--        pragma Debug(Test_Active_Iterator(Q1'access));
--        pragma Debug(Put_Line("Pre-Append: Expecting(from q2): nothing"));
--        pragma Debug(Test_Active_Iterator(Q2'access));
--       Append(Q2, Global_Items(1));
--        pragma Debug(Put_Line("Post-Append: Expecting(from q1): 9"));
--        pragma Debug(Test_Active_Iterator(Q1'access));
--        pragma Debug(Put_Line("Post-Append: Expecting(from q1): 1"));
--        pragma Debug(Test_Active_Iterator(Q2'access));
--       Append(Q2, Global_Items(2));
--       Append(Q2, Global_Items(3));
--       Append(Q2, Global_Items(4));
--       Assertion(Location(Q2, Global_Items(1)'1') = 1, "** P32: Queue location is not correct");
--       Assertion(Location(Q2, Global_Items(1)'2') = 2, "** P33: Queue location is not correct");
--       Assertion(Location(Q2, Global_Items(1)'4') = 4, "** P34: Queue location is not correct");
--       Remove(Q2, 1);
--       Remove(Q2, 2);
--       Remove(Q2, 2);
--       Assertion((Length(Q2) = 1), "** P35: Queue length is not correct");
--       Assertion((Front(Q2) = Global_Items(2)), "** P36: Queue front is not correct");
--       Remove(Q2, 1);
--       Assertion((Length(Q2) = 0), "** P37: Queue length is not correct");
--    end Test_User_Defined;

    procedure Test_Passive_Iterator (L : access Container'Class) is
       PIter : aliased Passive_Iterator(L);
       Success : Boolean;
    begin
       Success := Mod_op(PIter'access); -- just discard Success for now..
    end Test_Passive_Iterator;

   queue_b_p1, Queue_b_P2 : aliased Root_Bounded_Queues.Bnd_Queue;
   queue_d_p1, Queue_d_P2 : aliased Root_Dynamic_Queues.Dyn_Queue;
   queue_u_p1, Queue_U_P2 : aliased Root_Unbounded_Queues.Unb_Queue;
--    queue_b_u1, Queue_b_u2 : aliased User_Bound_Queues.Bnd_Queue;
--    queue_d_u1, Queue_d_u2 : aliased User_Dynamic_Queues.Dyn_Queue;
--    queue_u_u1, Queue_U_u2 : aliased User_Unbounded_Queues.Unb_Queue;

begin
  Put_Line("Starting queue tests");

  Put_Line("...Bounded Queue");
  test_primitive(queue_b_p1, queue_b_p2);
--  Test_User_defined(queue_b_u1, queue_b_u2);

  Put_Line("...Dynamic Queue");
  test_primitive(queue_d_p1, queue_d_p2);
--  Test_User_defined(queue_d_u1, queue_d_u2);

  Put_Line("...Unbounded Queue");
  test_primitive(queue_u_p1, queue_u_p2);
--  Test_User_defined(queue_u_u1, queue_u_u2);

   Put_Line("...Queue Active Iterator");
   Put_Line("   Bounded:");
   test_active_iterator(Queue_b_P1'access);
   Put_Line("   Dynamic:");
   test_active_iterator(Queue_d_P1'access);
   Put_Line("   Unbounded:");
   test_active_iterator(Queue_u_P1'access);

   Put_Line("...Queue Passive Iterator");
   Put_Line("   Bounded:");
   test_passive_iterator(Queue_b_P1'Access);
   Put_Line("   Dynamic:");
   test_passive_iterator(Queue_d_P1'Access);
   Put_Line("   Unbounded:");
   test_passive_iterator(Queue_u_P1'Access);

  Assertion((Front(Queue_b_p1) = '9'), "** M01: Queue front is not correct");
  Assertion((Length(Queue_b_p2) = 0), "** M02: Queue length is not correct");
--  Assertion((Front(Queue_b_u1) = Global_Items(9)), "** M03: Queue front is not correct");
--  Assertion((Length(Queue_b_u2) = 0), "** M04: Queue length is not correct");
  Assertion((Front(Queue_d_p1) = '9'), "** M05: Queue front is not correct");
  Assertion((Length(Queue_d_p2) = 0), "** M06: Queue length is not correct");
--  Assertion((Front(Queue_d_u1) = Global_Items(9)), "** M07: Queue front is not correct");
--  Assertion((Length(Queue_d_u2) = 0), "** M08: Queue length is not correct");
  Assertion((Front(Queue_u_p1) = '9'), "** M09: Queue front is not correct");
  Assertion((Length(Queue_u_p2) = 0), "** M10: Queue length is not correct");
--  Assertion((Front(Queue_u_u1) = Global_Items(9)), "** M11: Queue front is not correct");
--  Assertion((Length(Queue_u_u2) = 0), "** M12: Queue length is not correct");

  Assertion(Available(Queue_B_P1) = 99 , "** M13: Available space not correct");
  Assertion(Available(Queue_B_P2) =100 , "** M14: Available space not correct");
--  Assertion(Available(Queue_B_U1) = 99 , "** M15: Available space not correct");
--  Assertion(Available(Queue_B_U1) =100 , "** M16: Available space not correct");

  Put_Line("Completed queue tests");

end Queue_Test;
