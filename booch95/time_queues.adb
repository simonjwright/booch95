with Ada.Calendar;
with Ada.Text_Io;
with Queues_For_Timing;
procedure Time_Queues is
  B : aliased Queues_For_Timing.B.Bnd_Queue;
  D : aliased Queues_For_Timing.D.Dyn_Queue;
  U : aliased Queues_For_Timing.U.Unb_Queue;
  B_Iter : aliased Queues_For_Timing.C.Passive_Iterator (B'Access);
  D_Iter : aliased Queues_For_Timing.C.Passive_Iterator (D'Access);
  U_Iter : aliased Queues_For_Timing.C.Passive_Iterator (U'Access);
  procedure Iterate is
    Total : Integer;
    procedure Apply (Elem : Integer; Ok : out Boolean) is
    begin
      Total := Total + Elem;
      Ok := True;
    end Apply;
    function Q_Application is new Queues_For_Timing.C.Visit (Apply);
    Result : Boolean;
    Start : Ada.Calendar.Time;
    Taken : Duration;
    use type Ada.Calendar.Time;
  begin

    Total := 0;
    Start := Ada.Calendar.Clock;
    Result := Q_Application (B_Iter'Access);
    Taken := Ada.Calendar.Clock - Start;
    Ada.Text_Io.Put_Line
       (".. bounded queue took" & Taken'Img & " sec, sum" & Total'Img);

    Total := 0;
    Start := Ada.Calendar.Clock;
    Result := Q_Application (D_Iter'Access);
    Taken := Ada.Calendar.Clock - Start;
    Ada.Text_Io.Put_Line
       (".. dynamic queue took" & Taken'Img & " sec, sum" & Total'Img);

    Total := 0;
    Start := Ada.Calendar.Clock;
    Result := Q_Application (U_Iter'Access);
    Taken := Ada.Calendar.Clock - Start;
    Ada.Text_Io.Put_Line
       (".. unbounded queue took" & Taken'Img & " sec, sum" & Total'Img);

  end Iterate;
  procedure Time (N : Integer) is
  begin
    Ada.Text_Io.Put_Line
       ("timing iteration over containers of length" & N'Img);
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
exception
  when others =>
    Ada.Text_Io.Put_Line ("oops");
end Time_Queues;
