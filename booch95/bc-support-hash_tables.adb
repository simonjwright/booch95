-- Copyright (C) 1994-2000 Grady Booch and Simon Wright.
-- All Rights Reserved.
--
--      This program is free software; you can redistribute it
--      and/or modify it under the terms of the Ada Community
--      License which comes with this Library.
--
--      This program is distributed in the hope that it will be
--      useful, but WITHOUT ANY WARRANTY; without even the implied
--      warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--      PURPOSE. See the Ada Community License for more details.
--      You should have received a copy of the Ada Community
--      License with this library, in the file named "Ada Community
--      License" or "ACL". If not, contact the author of this library
--      for a copy.
--

-- $Id$

with BC.Support.Exceptions;

package body BC.Support.Hash_Tables is


  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Support.Tables");


  package body Tables is

    
    function "=" (L, R : Table) return Boolean is
    begin
      -- optimisation if L, R are the same Table?
      if L.Size = R.Size then
	for B in 1 .. Buckets loop
	  for Index in 1 .. Items.Length (L.Items (B).all) loop
	    declare
	      This_Item : Items.Item 
		 renames Items.Item_At (L.Items (B).all, Index);
	      use type Values.Value;
	    begin
	      if not Is_Bound (R, This_Item)
		 or else not (Values.Item_At (L.Values (B).all, Index)
			      = Values.Value'(Value_Of (R, This_Item))) then
		return False;
	      end if;
	    end;
	  end loop;
	end loop;
	return True;
      else
	return False;
      end if;
    end "=";


    procedure Clear (T : in out Table) is
    begin
      for B in 1 .. Buckets loop
	Items.Clear (T.Items (B).all);
	Values.Clear (T.Values (B).all);
	T.Size := 0;
      end loop;
    end Clear;


    procedure Bind (T : in out Table; I : Items.Item; V : Values.Value) is
      Bucket : constant Positive := (Items.Hash (I) mod Buckets) + 1;
    begin
      Assert (Items.Location (T.Items (Bucket).all, I, 1) = 0,
	      BC.Duplicate'Identity,
	      "Bind",
	      BSE.Duplicate);
      Items.Insert (T.Items (Bucket).all, I);
      Values.Insert (T.Values (Bucket).all, V);
      T.Size := T.Size + 1;
    end Bind;


    procedure Rebind (T : in out Table; I : Items.Item; V : Values.Value) is
      Bucket : constant Positive := (Items.Hash (I) mod Buckets) + 1;
      Index : constant Natural := Items.Location (T.Items (Bucket).all, I, 1);
    begin
      Assert (Index /= 0,
	      BC.Not_Found'Identity,
	      "Rebind",
	      BSE.Missing);
      Values.Replace (T.Values (Bucket).all, Index, V);
    end Rebind;


    procedure Unbind (T : in out Table; I : Items.Item) is
      Bucket : constant Positive := (Items.Hash (I) mod Buckets) + 1;
      Index : constant Natural := Items.Location (T.Items (Bucket).all, I, 1);
    begin
      Assert (Index /= 0,
	      BC.Not_Found'Identity,
	      "Unbind",
	      BSE.Missing);
      Items.Remove (T.Items (Bucket).all, Index);
      Values.Remove (T.Values (Bucket).all, Index);
      T.Size := T.Size - 1;
    end Unbind;

    function Extent (T : Table) return Natural is
    begin
      return T.Size;
    end Extent;


    function Is_Bound (T : Table; I : Items.Item) return Boolean is
      Bucket : constant Positive := (Items.Hash (I) mod Buckets) + 1;
    begin
      return Items.Location (T.Items (Bucket).all, I, 1) /= 0;
    end Is_Bound;


    function Value_Of (T : Table; I : Items.Item) return Values.Value is
      Bucket : constant Positive := (Items.Hash (I) mod Buckets) + 1;
      Index : constant Natural := Items.Location (T.Items (Bucket).all, I, 1);
    begin
      Assert (Index /= 0,
	      BC.Not_Found'Identity,
	      "Value_Of",
	      BSE.Missing);
      return Values.Item_At (T.Values (Bucket).all, Index);
    end Value_Of;


    function Value_Of (T : Table; I : Items.Item) return Values.Value_Ptr is
      Bucket : constant Positive := (Items.Hash (I) mod Buckets) + 1;
      Index : constant Natural := Items.Location (T.Items (Bucket).all, I, 1);
    begin
      Assert (Index /= 0,
	      BC.Not_Found'Identity,
	      "Value_Of",
	      BSE.Missing);
      return Values.Item_At (T.Values (Bucket).all, Index);
    end Value_Of;


    function Item_Bucket
       (T : Table; Bucket : Positive) return Items.Item_Container_Ptr is
    begin
      Assert (Bucket <= Buckets,
	      BC.Container_Error'Identity,
	      "Item_Bucket");
      return T.Items (Bucket);
    end Item_Bucket;


    function Value_Bucket
       (T : Table; Bucket : Positive) return Values.Value_Container_Ptr is
    begin
      Assert (Bucket <= Buckets,
	      BC.Container_Error'Identity,
	      "Value_Bucket");
      return T.Values (Bucket);
    end Value_Bucket;


    procedure Initialize (T : in out Table) is
    begin
      for B in 1 .. Buckets loop
	T.Items (B) := new Items.Item_Container;
	T.Values (B) := new Values.Value_Container;
      end loop;
    end Initialize;


    procedure Adjust (T : in out Table) is
    begin
      for B in 1 .. Buckets loop
	T.Items (B) := Items.Create (T.Items (B).all);
	T.Values (B) := Values.Create (T.Values (B).all);
      end loop;
    end Adjust;


    procedure Finalize (T : in out Table) is
    begin
      for B in 1 .. Buckets loop
	Items.Free (T.Items (B));
	Values.Free (T.Values (B));
      end loop;
    end Finalize;


  end Tables;
  

end BC.Support.Hash_Tables;
