with Ada.Exceptions;
use  Ada.Exceptions;

package body Sequential_Fixed_Block is


  function Value_At( Location : System.Address ) return SSE.Storage_Offset is
  begin
    return Link.To_Pointer(Location).all;
  end Value_At;

  procedure Place( Value : in SSE.Storage_Offset;  Location : in System.Address ) is
  begin
    Link.To_Pointer(Location).all := Value;
  end Place;


  use type SSE.Storage_Offset;


  procedure Allocate(	Pool             : in out Storage_Pool;
                      Storage_Address  :    out System.Address;
                      Allocation       : in     SSE.Storage_Count;
                      Alignment        : in     SSE.Storage_Count  ) is
  begin
    if Pool.Next_Free /= Null_Pointer then -- at least one is available
      Storage_Address := Pool.Content(Pool.Next_Free)'Address;
      Pool.Next_Free := Value_At( Storage_Address );
    else -- none available
      raise Storage_Error;
    end if;
  end Allocate;


  procedure Deallocate(	Pool            : in out Storage_Pool;
                        Storage_Address : in     System.Address;
                        Allocation      : in     SSE.Storage_Count;
                        Alignment       : in     SSE.Storage_Count ) is
  begin
    -- Note: we are not checking to see if previously deallocated!
    -- place index of next free element into this space (treated as a storage_offset value)
    Place( Pool.Next_Free, Storage_Address );
    -- update next_free index to indicate this newly-freed space
    Pool.Next_Free := Storage_Address - Pool.Content'Address + Pool.Content_Alignment_Offset + 1;
  end Deallocate;


  function Storage_Size( Pool : Storage_Pool ) return SSE.Storage_Count is
  begin
    return Pool.Size;
  end Storage_Size;


  procedure Initialize( Pool : in out Storage_Pool ) is
    Next_Out : SSE.Storage_Offset;
  begin
    Pool.Aligned_Element_Size := ((Pool.Element_Size + Pool.Alignment - 1) / Pool.Alignment) * Pool.Alignment;
    if (SSE.Storage_Offset'Size / System.Storage_Unit) > Pool.Aligned_Element_Size then
      Raise_Exception( Representation_Error'Identity,
                      "Bookkeeping would overlap allocated elements." );
    end if;
    if Pool.Content'Address mod Pool.Alignment /= 0 then  -- Content(1) is not properly aligned
      Pool.Content_Alignment_Offset := Pool.Alignment - (Pool.Content'Address mod Pool.Alignment);
    else -- Content(1) is aligned properly
      Pool.Content_Alignment_Offset := 0;
    end if;
    Next_Out := Pool.Content'First + Pool.Content_Alignment_Offset;
    Pool.Next_Free := Null_Pointer;
    while Next_Out <= Pool.Size - Pool.Aligned_Element_Size + 1 loop
      Place( Pool.Next_Free, Pool.Content(Next_Out)'Address );
      Pool.Next_Free := Next_Out;
      Next_Out := Next_Out + Pool.Aligned_Element_Size;
    end loop;
  end Initialize;


  function Available_Allocations( Pool : Storage_Pool ) return Natural is
    Result : Natural := 0;
    Ptr    : SSE.Storage_Offset;
  begin
    Ptr := Pool.Next_Free;
    while Ptr /= Null_Pointer loop
      Result := Result + 1;
      Ptr := Value_At( Pool.Content(Ptr)'Address );
    end loop;
    return Result;
  end Available_Allocations;


  function Empty( Pool : Storage_Pool ) return Boolean is
  begin
    return Pool.Next_Free = Null_Pointer;
  end Empty;


end Sequential_Fixed_Block;
