-------------------------------------------------------------------------------
--                       storage_pool_handler.ads                            --
--                                                                           --
-- Purposes: Manage memory resources. As a result memory leakage problem     --
--           will be thing of the past.                                      --
--                                                                           --
-- Anh Vo - 01 March 2000                                                    --
--                                                                           --
-- C/C++ is for money. Java is for fame and money. Ada is for reliability.   --
--                                                                           --
-------------------------------------------------------------------------------

with System.Storage_Pools;
with System.Storage_Elements;

package Storage_Pool_Handler is

   -- Indicates storage pool problems mainly for storage exhaustion
   Storage_Pool_Error : exception;

   -- User did not specify his identity
   Unidentified_User : exception;

   ----------------------------------------------------------------------------
   -- Variable memory blocks are handled by this pool.                       --
   --                                                                        --
   -- Below shows the usage of General Pool.                                 --
   -- type Some_Type is ...;                                                 --
   -- type Some_Type_Access is access Some_Type;                             --
   --                                                                        --
   -- Client_Name : aliased String := "Single_Linked_List";                  --
   -- Client_Pool : Storage_Pool_Handler.General_Pool (                      --
   --                  Size => 4_000,  -- could be read from a config file   --
   --                  User => Client_Name'Access);                          --
   -- for Some_Type_Access'Storage_Pool use Client_Pool;                     --
   --                                                                        --
   -- Object_Access : Some_Type_Access := new Some_Type;                     --
   --                                                                        --
   -- Note!!! Some_Type must have the minimum of 64 bits in size.            --
   ----------------------------------------------------------------------------
   type General_Pool (Size : System.Storage_Elements.Storage_Count;
                      User : access String) is new
                        System.Storage_Pools.Root_Storage_Pool with private;

   procedure Allocate (
      Pool            : in out General_Pool;
      Storage_Address :    out System.Address;
      Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
      Alignment       : in System.Storage_Elements.Storage_Count);

   procedure Deallocate (
      Pool            : in out General_Pool;
      Storage_Address : in     System.Address;
      Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
      Alignment       : in System.Storage_Elements.Storage_Count);

   function Storage_Size (Pool : General_Pool)
                            return System.Storage_Elements.Storage_Count;

   function Client_Id (Pool : General_Pool) return String;


   ----------------------------------------------------------------------------
   -- Detailed memory blocks size are handled by this pool. The advantage    --
   -- of fixed block size is that the execution speed is faster than both    --
   -- General Pool and Heap mechanism.                                       --
   --                                                                        --
   -- Note!!  Each access type has its own pool. Do not use more than one    --
   --         access type per pool.                                          --
   --                                                                        --
   -- Below shows the usage of Detailed Pool.                                --
   -- type Some_Type is ...;                                                 --
   -- type Some_Type_Access is access Some_Type;                             --
   --                                                                        --
   -- Client_Name : aliased String := "Integer_Access";                      --
   -- Client_Pool : Storage_Pool_Handler.Detailed_Pool (                     --
   --                  Size => 4_000,  -- could be read from a config file   --
   --                  Max => Some_Type'Max_Size_In_Storage_Elements,        --
   --                  User => Client_Name'Access);                          --
   -- for Some_Type_Access'Storage_Pool use Client_Pool;                     --
   --                                                                        --
   -- Object_Access : Some_Type_Access := new Some_Type;                     --
   --                                                                        --
   -- Note!!! Some_Type must have the minimum of 32 bits in size.            --
   ----------------------------------------------------------------------------
   type Detailed_Pool (Max  : System.Storage_Elements.Storage_Count;
                       Size : System.Storage_Elements.Storage_Count;
                       User : access String) is new
                         System.Storage_Pools.Root_Storage_Pool with private;

   procedure Allocate (
         Pool                     : in out Detailed_Pool;
         Storage_Address          :    out System.Address;
         Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
         Alignment                : in System.Storage_Elements.Storage_Count);

   procedure Deallocate (
         Pool                     : in out Detailed_Pool;
         Storage_Address          : in System.Address;
         Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
         Alignment                : in System.Storage_Elements.Storage_Count);

   function Storage_Size (Pool : in Detailed_Pool )
                               return System.Storage_Elements.Storage_Count;

   function User_Id (Pool : Detailed_Pool) return String;

   -- Free memory available for use
   function Memory_Left (Pool : Detailed_Pool)
                               return System.Storage_Elements.Storage_Count;

   -- General status of the memory pool
   function Status (Pool : Detailed_Pool) return String;

private

   Max_Alignment : constant := System.Word_Size / System.Storage_Unit;

   type User_Storage_Array is array
        (System.Storage_Elements.Storage_Count range <>) of
                                   System.Storage_Elements.Storage_Element;
   for User_Storage_Array'Alignment use Max_Alignment;

   --------------------------- General_Pool -----------------------------------
    type General_Pool (Size : System.Storage_Elements.Storage_Count;
                       User : access String ) is new
      System.Storage_Pools.Root_Storage_Pool with record
      Data       : User_Storage_Array (1 .. Size);
      Addr_Index : System.Storage_Elements.Storage_Count := 1;
      Addr_Head  : System.Address := System.Null_Address;
   end record;

   Procedure Initialize (Pool : in out General_Pool);
   Procedure Finalize (Pool : in out General_Pool);

   -- the following two subprograms are for internal test only.
   function Storage_Available (Pool : General_Pool)
                      return System.Storage_Elements.Storage_Count;

   function Largest_Block (Pool : General_Pool)
                            return System.Storage_Elements.Storage_Count;


   ---------------------------- Detailed_Pool ---------------------------------
   type Detailed_Pool (Max  : System.Storage_Elements.Storage_Count;
                       Size : System.Storage_Elements.Storage_Count;
                       User : access String) is new
                                System.Storage_Pools.Root_Storage_Pool with
      record
         Data : User_Storage_Array (1 .. Size);
         Head_Keeper : System.Address := System.Null_Address;
         Block_Count : Natural := 0;
      end record;

   procedure Initialize (Pool : in out Detailed_Pool);
   procedure Finalize (Pool : in out Detailed_Pool);

   -- the subprogram below is for internal test only.
   function Block_Remain (Pool : Detailed_Pool) return Natural;

end Storage_Pool_Handler;
