package body BC.Support.Unmanaged_Storage is

  type Default_Access_Type is access Integer;  -- arbitrary designated subtype

  Default_Pool : SSP.Root_Storage_Pool'Class renames Default_Access_Type'Storage_Pool;
    -- the conversions to the class-wide value in the bodies below shouldn't (IMHO) be necessary


  procedure Allocate( The_Pool                 : in out Pool;
                      Storage_Address          :    out System.Address;
                      Size_In_Storage_Elements : in     SSE.Storage_Count;
                      Alignment                : in     SSE.Storage_Count ) is
  begin
    SSP.Allocate( SSP.Root_Storage_Pool'Class(Default_Pool), Storage_Address, Size_In_Storage_Elements, Alignment );
  end Allocate;


  procedure Deallocate( The_Pool                 : in out Pool;
                        Storage_Address          : in     System.Address;
                        Size_In_Storage_Elements : in     SSE.Storage_Count;
                        Alignment                : in     SSE.Storage_Count ) is
  begin
    SSP.Deallocate( SSP.Root_Storage_Pool'Class(Default_Pool), Storage_Address, Size_In_Storage_Elements, Alignment );
  end Deallocate;


  function Storage_Size( This : Pool ) return SSE.Storage_Count is
  begin
    return SSP.Storage_Size( SSP.Root_Storage_Pool'Class(Default_Pool) );
  end Storage_Size;


end BC.Support.Unmanaged_Storage;


