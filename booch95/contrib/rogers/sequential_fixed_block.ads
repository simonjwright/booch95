-- (C) Copyright 1997 by Patrick Rogers of Software Arts & Sciences
-- progers@classwide.com
-- http://www.classwide.com
--
-- Rights to use, distribute or modify this package in any way is hereby
-- granted, provided this header is kept unchanged in all versions.
-- Additionnal headers may be added. If you make a valuable addition,
-- please keep us informed by sending a message to progers@classwide.com
--
-- THIS ADA LIBRARY IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
-- WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
-- MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.


with System.Storage_Pools;
with System.Storage_Elements;
with System.Address_To_Access_Conversions;

package Sequential_Fixed_Block is

  pragma Elaborate_Body;

  package SSE renames System.Storage_Elements;
  package SSP renames System.Storage_Pools;

  Representation_Error : exception;

  -- All elements allocated must be the same size, and have the same alignment;
  -- different types are allowed as long as the above holds.

  type Storage_Pool( Size         : SSE.Storage_Count;
                     Element_Size : SSE.Storage_Count;
                     Alignment    : SSE.Storage_Count ) is
    new SSP.Root_Storage_Pool with private;

  procedure Allocate( Pool             : in out Storage_Pool;
                      Storage_Address  :    out System.Address;
                      Allocation       : in     SSE.Storage_Count;
                      Alignment        : in     SSE.Storage_Count  );

  procedure Deallocate(	Pool            : in out Storage_Pool;
                        Storage_Address : in     System.Address;
                        Allocation      : in     SSE.Storage_Count;
                        Alignment       : in     SSE.Storage_Count );
	
  function Storage_Size( Pool : Storage_Pool ) return SSE.Storage_Count;

  function Available_Allocations( Pool : Storage_Pool ) return Natural;

  function Empty( Pool : Storage_Pool ) return Boolean;

private

  type Storage_Pool( Size         : SSE.Storage_Count;
                     Element_Size : SSE.Storage_Count;
                     Alignment    : SSE.Storage_Count ) is
    new SSP.Root_Storage_Pool with
      record
        Content                  : SSE.Storage_Array( 1 .. Size );
        Next_Free                : SSE.Storage_Offset;
        Aligned_Element_Size     : SSE.Storage_Offset;
        Content_Alignment_Offset : SSE.Storage_Offset;
      end record;

  procedure Initialize( Pool : in out Storage_Pool );


  -- A Storage_Array is a contiguous chunk of memory (by definition in Ada).
  -- Each pool has a storage_array of the pool size requested, called Content.
  -- Content is allocated in chunks according to the element size, modulo
  -- alignment requirements.  A "free list" is maintained within the Content
  -- storage_array, using the indexes as "pointers".  Whenever space is
  -- manipulated, Next_Free is treated as the "head" of a linked list; thus the
  -- "link" of the newly-freed space has the value of Next_Free written into
  -- it, by treating the returning address as a pointer to a Storage_Offset
  -- value.  Then Next_Free is updated to "point" to this new space.  Value
  -- zero corresponds to a null pointer.

  Null_Pointer : constant SSE.Storage_Offset := 0;

  package Link is
    new System.Address_To_Access_Conversions( SSE.Storage_Offset );
    -- Using Storage_Element would seem attractive, but
    -- we don't know that Storage_Element values support a sufficient
    -- range to express pool sizes.

  function Value_At( Location : System.Address ) return SSE.Storage_Offset;

  procedure Place( Value : in SSE.Storage_Offset;  Location : in System.Address );

  pragma Inline( Value_At, Place );

end Sequential_Fixed_Block;

