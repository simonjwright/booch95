--  This is a demonstration of the ability for multiple tasks to
--  simultaneously iterate over an unchanging unbounded collection, in
--  response to feature request #3
--  (https://sourceforge.net/p/booch95/feature-requests/3/).

with Ada.Exceptions;
with Ada.Text_IO;
with Concurrent_Access_Demo_Support;
procedure Concurrent_Access_Demo is
   package CADT renames Concurrent_Access_Demo_Support;
   Collection : CADT.Collections.Collection;
   task type Accessor is
      entry Start (ID : Character);
   end Accessor;
   task body Accessor is
      ID : Character;
   begin
      accept Start (ID : Character) do
         Accessor.ID := ID;
      end Start;
      delay 0.01; -- so the other task can be started
      declare
         It : CADT.Abstract_Containers.Iterator'Class
           := CADT.Collections.New_Constant_Collection_Iterator (Collection);
         Expected : Positive;
      begin
         for J in 1 .. 1_000 loop
            Ada.Text_IO.Put (ID);
            CADT.Abstract_Containers.Reset (It);
            Expected := 1;
            while not CADT.Abstract_Containers.Is_Done (It)
            loop
               declare
                  Current : constant Positive :=
                    CADT.Abstract_Containers.Current_Item (It);
               begin
                  if Current /= Expected
                  then
                     Ada.Exceptions.Raise_Exception
                       (Program_Error'Identity,
                        "expected" & Expected'Img & ", got" & Current'Img);
                  end if;
               end;
               Expected := Expected + 1;
               CADT.Abstract_Containers.Next (It);
            end loop;
         end loop;
      end;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Accessor;
   A1, A2 : Accessor;
begin
   for J in 1 .. 10_000 loop
      CADT.Collections.Append (Collection, J);
   end loop;
   A1.Start ('o');
   A2.Start ('x');
end Concurrent_Access_Demo;
