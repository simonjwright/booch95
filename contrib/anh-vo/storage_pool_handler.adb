-------------------------------------------------------------------------------
--                       storage_pool_handler.adb                            --
--                                                                           --
-- Purposes: Manages memory resources. As a result memory leakage problem    --
--           will be thing of the past.                                      --
--                                                                           --
-- Anh Vo - 01 March 2000                                                    --
--                                                                           --
-- C/C++ is for money. Java is for fame and money. Ada is for reliability.   --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Text_IO;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;

package body Storage_Pool_Handler is

   use Ada;
   use Text_IO;

   use type System.Address;
   use type System.Storage_Elements.Storage_Count;

   Package_Name : constant String := "Storage_Pool_Handler.";


   --  used by General Pool to link memory together
   type Address_Linker is
      record
         Next_Address  : System.Address := System.Null_Address;
         Size_Elements : System.Storage_Elements.Storage_Count;
      end record;

   --  storage elements needed by General Pool
   Link_Storage : constant System.Storage_Elements.Storage_Count :=
                             Address_Linker'Max_Size_In_Storage_Elements;

   package Address_Access_Conversion is new
                     System.Address_To_Access_Conversions (Address_Linker);


   --  used by Detailed Pool to link the memory together
   type Holder is
      record
         Next_Address : System.Address := System.Null_Address;
      end record;

   --  storage elements needed by Holder record.
   Holder_Storage : constant System.Storage_Elements.Storage_Count :=
                                      Holder'Max_Size_In_Storage_Elements;

   package Address_Access_Conv is new
               System.Address_To_Access_Conversions (Holder);


   function Aligned (
              Size      : System.Storage_Elements.Storage_Count;
              Alignment : System.Storage_Elements.Storage_Count)
                            return System.Storage_Elements.Storage_Count;
   function Aligned (
              Size      : System.Storage_Elements.Storage_Count;
              Alignment : System.Storage_Elements.Storage_Count)
                            return System.Storage_Elements.Storage_Count is
   begin
      if Size rem Alignment /= 0 then
         return Size + Alignment - (Size mod Alignment);
      else
         return Size;
      end if;
   end Aligned;


   procedure Allocate (
         Pool            : in out General_Pool;
         Storage_Address :    out System.Address;
         Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
         Alignment       : in System.Storage_Elements.Storage_Count) is

      Previous : System.Address := System.Null_Address;
      Current  : System.Address := Pool.Addr_Head;
      Desired_Size : System.Storage_Elements.Storage_Count :=
                                               Size_In_Storage_Elements;

   begin   -- Allocate

      while Current /= System.Null_Address loop
         if Address_Access_Conversion.To_Pointer (Current).Size_Elements =
                                                      Desired_Size then

            --  select the matched memory block
            if Previous = System.Null_Address then
               Pool.Addr_Head :=
                 Address_Access_Conversion.To_Pointer (Current).Next_Address;
            else
               --  examine the next memory block
               Address_Access_Conversion.To_Pointer (Previous).Next_Address :=
                 Address_Access_Conversion.To_Pointer (Current).Next_Address;
            end if;

            Storage_Address := Current;
            return;    -- mission complete

         else   -- check the next address of the link

            Previous := Current;
            Current :=
              Address_Access_Conversion.To_Pointer (Current).Next_Address;
         end if;
      end loop;

      --  nothing found from storage reuse, grap storage elements from pool
      if Pool.Size - Pool.Addr_Index > Desired_Size then
         Storage_Address := Pool.Data (Pool.Addr_Index)'Address;
         Pool.Addr_Index := Pool.Addr_Index + Desired_Size;
      else
         Exceptions.Raise_Exception (Storage_Pool_Error'Identity,
           Message => Pool.User.all & "'s General Storage Pool is exhausted");
      end if;

   end Allocate;

   procedure Deallocate (
         Pool            : in out General_Pool;
         Storage_Address : in     System.Address;
         Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
         Alignment       : in System.Storage_Elements.Storage_Count) is

      New_Object : Address_Access_Conversion.Object_Pointer;
      Desired_Size : System.Storage_Elements.Storage_Count :=
                                               Size_In_Storage_Elements;

   begin   -- Deallocate

      New_Object := Address_Access_Conversion.To_Pointer (Storage_Address);

      New_Object.all := (Next_Address => Pool.Addr_Head,
                         Size_Elements => Desired_Size);

      Pool.Addr_Head := Storage_Address;

   end Deallocate;


   function Storage_Size (Pool : General_Pool)
                            return System.Storage_Elements.Storage_Count is
   begin
      return Pool.Size;
   end Storage_Size;


   function Client_Id (Pool : General_Pool) return String is
   begin
      return Pool.User.all;
   end Client_Id;

   function Storage_Available (Pool : General_Pool)
                      return System.Storage_Elements.Storage_Count is
   begin
      return Pool.Size - Pool.Addr_Index + 1;
   end Storage_Available;

   function Largest_Block (Pool : General_Pool)
                            return System.Storage_Elements.Storage_Count is
      Free_Block : System.Storage_Elements.Storage_Count := 0;
      Addr_Index : System.Address := Pool.Addr_Head;
      The_Object : Address_Access_Conversion.Object_Pointer;
   begin
      while Addr_Index /= System.Null_Address loop
         The_Object := Address_Access_Conversion.To_Pointer (Addr_Index);

         if The_Object.Size_Elements > Free_Block then
            Free_Block := The_Object.Size_Elements;
         else
            Addr_Index :=
              Address_Access_Conversion.To_Pointer (Addr_Index).Next_Address;
         end if;
      end loop;

      if Free_Block >= Pool.Size - Pool.Addr_Index + 1 then
         return Free_Block;
      else
         return Pool.Size - Pool.Addr_Index + 1;
      end if;
   end Largest_Block;

   procedure Initialize (Pool : in out General_Pool) is
   begin

      if Pool.Size < Link_Storage then
         --  fatal error - No go
         Exceptions.Raise_Exception (Storage_Pool_Error'Identity,
           Pool.User.all & "'s pool size specification is terribly wrong." &
           " The requirements are: Size must be at least" &
           System.Storage_Elements.Storage_Count'Image (Link_Storage) &
           " elements long");
      end if;

      if Pool.User.all = ""  then
         --  serious usage error
         Exceptions.Raise_Exception (Unidentified_User'Identity,
           "Pool User, you did not have a meaningful name");
      end if;

--      Put_Line (Pool.User.all & " user has the pool size of" &
--        System.Storage_Elements.Storage_Count'Image (Pool.Size));
   end Initialize;

   procedure Finalize (Pool : in out General_Pool) is
      Free_Count : System.Storage_Elements.Storage_Count := 0;
      Addr_Index : System.Address := Pool.Addr_Head;
      The_Object : Address_Access_Conversion.Object_Pointer;
   begin
      while Addr_Index /= System.Null_Address loop
         The_Object := Address_Access_Conversion.To_Pointer (Addr_Index);
         Free_Count := Free_Count + The_Object.Size_Elements;
         Addr_Index :=
           Address_Access_Conversion.To_Pointer (Addr_Index).Next_Address;
      end loop;

      if Free_Count /= Pool.Addr_Index - 1 then
         Put_Line (Standard_Output, "Memory Leakage Detected!!! " &
           Pool.User.all & "'s codes had memory leakage of" &
           System.Storage_Elements.Storage_Count'Image (
              Pool.Addr_Index - 1 - Free_Count) & " storage elements.");
      end if;

--      Put_Line (Pool.User.all & "'s " & "Free_Count = " &
--        System.Storage_Elements.Storage_Count'Image (Free_Count));
--      Put_Line (Pool.User.all & "'s " & "Addr_Index = " &
--        System.Storage_Elements.Storage_Count'Image (Pool.Addr_Index));

   end Finalize;


   ----------------------------------------------------------------------------
   -- Detailed memory blocks size are handled by this pool                   --
   ----------------------------------------------------------------------------
   procedure Allocate (
         Pool            : in out Detailed_Pool;
         Storage_Address :    out System.Address;
         Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
         Alignment       : in System.Storage_Elements.Storage_Count) is
   begin

      if Pool.Head_Keeper /= System.Null_Address then
         Storage_Address := Pool.Head_Keeper;
         Pool.Head_Keeper :=
           Address_Access_Conv.To_Pointer (Pool.Head_Keeper).Next_Address;
      else
         Exceptions.Raise_Exception (
           E => Storage_Pool_Error'Identity,
           Message => Pool.User.all & "'s Fixed Storage Pool is exhausted");
      end if;

   end Allocate;

   procedure Deallocate (
         Pool                     : in out Detailed_Pool;
         Storage_Address          : in System.Address;
         Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
         Alignment : in System.Storage_Elements.Storage_Count) is
   begin
      Address_Access_Conv.To_Pointer (Storage_Address).Next_Address :=
                                                              Pool.Head_Keeper;
      Pool.Head_Keeper := Storage_Address;
   end Deallocate;

   function Storage_Size (Pool : in Detailed_Pool)
                            return System.Storage_Elements.Storage_Count is
   begin
      return Pool.Size;
   end Storage_Size;

   function User_Id (Pool : Detailed_Pool) return String is
   begin
      return Pool.User.all;
   end User_Id;


   function Memory_Left (Pool : Detailed_Pool)
                               return System.Storage_Elements.Storage_Count is
      Memory_Remain : constant Natural := Block_Remain (Pool);
   begin
      return System.Storage_Elements.Storage_Count (Memory_Remain);
   end Memory_Left;

   function Status (Pool : Detailed_Pool) return String is
   begin
      return (Pool.User.all & " has" &
         Natural'Image (Block_Remain (Pool)) & " blocks free out of" &
         Natural'Image (Pool.Block_Count) & "Block or" &
         Natural'Image (Block_Remain (Pool) * Natural (Pool.Max)) &
         " out of " & Natural'Image (Pool.Block_Count * Natural (Pool.Max)));
   end Status;


   function Block_Remain (Pool : Detailed_Pool) return Natural is
      Count : Natural := 0;
      Temp_Addr : System.Address := Pool.Head_Keeper;
   begin
      while Temp_Addr /= System.Null_Address loop
         Count := Count + 1;
         Temp_Addr := Address_Access_Conv.To_Pointer (Temp_Addr).Next_Address;
      end loop;
      return Count;
   end Block_Remain;

   procedure Initialize (Pool : in out Detailed_Pool) is
      Temp_Addr : System.Address := System.Null_Address;
      Addr_Index : System.Storage_Elements.Storage_Count := 1;

   begin   -- Initialize

      if Pool.Max < Holder_Storage or else Pool.Size < Pool.Max then
         --  fatal error - No go
         Exceptions.Raise_Exception (Storage_Pool_Error'Identity,
           Pool.User.all & "'s pool size specification is terribly wrong." &
           " The requirements are: Size must be at least" &
           System.Storage_Elements.Storage_Count'Image (Holder_Storage) &
           " elements and Max must be at least" &
            System.Storage_Elements.Storage_Count'Image (Pool.Size) &
           " elements long");
      end if;

      if Pool.User.all = ""  then
         --  serious usage error
         Exceptions.Raise_Exception (Unidentified_User'Identity,
           "Pool user, you did not have a meaningful name");
      end if;

      while Pool.Size >= Addr_Index + Pool.Max - 1 loop
         Temp_Addr := Pool.Data (Addr_Index)'Address;
         Addr_Index := Addr_Index + Pool.Max;
         Address_Access_Conv.To_Pointer (Temp_Addr).Next_Address :=
                                                            Pool.Head_Keeper;
         Pool.Head_Keeper := Temp_Addr;
         Pool.Block_Count := Pool.Block_Count + 1;
      end loop;

--      Put_Line (Pool.User.all & " user has" &
--        Integer'Image (Pool.Block_Count) & " blocks with block Size of" &
--        System.Storage_Elements.Storage_Count'Image (Pool.Max) &
--        " for the total size of" &
--        System.Storage_Elements.Storage_Count'Image (Pool.Size));
   end Initialize;

   procedure Finalize (Pool : in out Detailed_Pool) is
   begin
      if Block_Remain (Pool) /= Pool.Block_Count then
         Put_Line (Standard_Output, "Memory Leakage Detected!!! " &
                     Pool.User.all & "'s codes had memory leakage of" &
           Natural'Image ((Pool.Block_Count -
                             Block_Remain (Pool)) * Integer (Pool.Max)) &
                                                      " storage elements.");
      end if;
   end Finalize;

end Storage_Pool_Handler;
