package body Sequential_Fixed_Block.Checked is


   use type SSE.Storage_Offset;


   procedure Deallocate (Pool            : in out Storage_Pool;
                         Storage_Address : in     System.Address;
                         Allocation      : in     SSE.Storage_Count;
                         Alignment       : in     SSE.Storage_Count) is
      Ptr : SSE.Storage_Offset;
      use type System.Address;
   begin
      Ptr := Pool.Next_Free;
      while Ptr /= Null_Pointer loop
         if Pool.Content (Ptr)'Address = Storage_Address then
            --  previously deallocated
            return;
         end if;
         Ptr := Value_At (Pool.Content (Ptr)'Address);
      end loop;
      --  not Previously_Deallocated
      --  place index of next free element into this space (treated as
      --  a storage_offset value)
      Place (Pool.Next_Free, Storage_Address);
      --  update next_free index to indicate this newly-freed space
      Pool.Next_Free :=
        Storage_Address
        - Pool.Content'Address
        + Pool.Content_Alignment_Offset
        + 1;
   end Deallocate;


end Sequential_Fixed_Block.Checked;
