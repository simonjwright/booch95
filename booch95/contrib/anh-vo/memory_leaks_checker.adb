with Ada.Exceptions;
with Ada.Text_Io;
with Ada.Unchecked_Deallocation;

with Storage_Pool_Handler;

procedure Memory_Leaks_Checker is

   use Ada;
   use Text_Io;
   use Exceptions;
      
   Pool_Name : aliased String := "Integer_Access";

   Integer_Access_Pool : Storage_Pool_Handler.Detailed_Pool (
     Size => 100,
     Max => Integer'Max_Size_In_Storage_Elements,
     User => Pool_Name'Access);

   type Integer_Access is access Integer;
   for Integer_Access'Storage_Pool use Integer_Access_Pool;

   procedure Free is new Unchecked_Deallocation (Integer, Integer_Access);   

begin

   Put_Line ("Memory_Leaks_Checker testing starts");

   declare
      Int_1 : Integer_Access;
      Int_2 : Integer_Access;
      Int_3 : Integer_Access;
   begin
      Int_1 := new Integer' (111);
      Int_2 := new Integer' (222);
      Int_3 := new Integer' (333);
      
      -- Memory leaks will be detected if the statement below is
      -- commented out to simulate the condition where the memory
      -- was not free.
      Free (Int_1);  Free (Int_2);  Free (Int_3);      
   end;

   Put_Line ("Memory_Leaks_Checker testing completed");

exception
   when Error : others =>
      Put_Line ("Memory_Leaks_Checker Failed because of " &
                                       Exception_Information (Error));

end Memory_Leaks_Checker;