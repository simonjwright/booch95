with System.Storage_Elements;

package Sequential_Fixed_Block.Checked is

  -- Instances of Unchecked_Deallocation, which call Deallocate, will check that
  -- prior calls with the same access value do not attempt to deallocate the
  -- same access value, thereby preventing heap corruption.

  pragma Elaborate_Body;

  package SSE renames System.Storage_Elements;

  type Storage_Pool is
    new Sequential_Fixed_Block.Storage_Pool with null record;

  procedure Deallocate(	Pool            : in out Storage_Pool;
                        Storage_Address : in     System.Address;
                        Allocation      : in     SSE.Storage_Count;
                        Alignment       : in     SSE.Storage_Count );
	
end Sequential_Fixed_Block.Checked;

