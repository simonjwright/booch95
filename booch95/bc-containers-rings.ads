-- Copyright (C) 1994-1999 Grady Booch and Simon Wright.
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

generic
package BC.Containers.Rings is

  type Ring is abstract new Container with private;

  -- A ring denotes a sequence in which items may be added and removed
  -- from the top of a circular structure. Since this structure has no
  -- beginning or ending, a client can mark one particular item to
  -- designate a point of reference in the structure.

--| //  The C++ Booch Components (Version 2.3)
--| //  (C) Copyright 1990-1994 Grady Booch. All Rights Reserved.
--| //
--| //  BCRing.h
--| //
--| //  This file contains the declaration of the ring abstract base class
--| //  and its iterators.
--|
--| #ifndef BCRING_H
--| #define BCRING_H 1
--|
--| #include "BCType.h"
--|
--| template<class Item>
--| class BC_TRingActiveIterator;
--|
--| template<class Item>
--| class BC_TRingPassiveIterator;
--|
--| template<class Item, class Structure>
--| class BC_TPersist;
--|
--| enum BC_Direction {BC_kForward, BC_kReverse};

  type Direction is (Forward, Backward); -- XXX why not clockwise, anticlockwise?

--|
--| // Ring abstract base class
--|
--| template<class Item>
--| class BC_TRing {
--| public:
--|
--|   BC_TRing();
--|   BC_TRing(const BC_TRing<Item>&);
--|   virtual ~BC_TRing();
--|
--|   virtual BC_TRing<Item>& operator=(const BC_TRing<Item>&);
--|   virtual BC_Boolean operator==(const BC_TRing<Item>&) const;

  function Are_Equal (Left, Right : Ring'Class) return Boolean;
  -- Return True if and only if both rings have the same extent and the
  -- same items in the same order; return False otherwise. The
  -- identity of the top and mark of both rings does not participate
  -- in this test of equality.
  -- Can't call this "=" because of the standard one for Ring.

  procedure Copy (From : Ring'Class; To : in out Ring'Class);
  -- This operation MUST be called for dissimilar Rings in place of
  -- assignment.

--|   BC_Boolean operator!=(const BC_TRing<Item>&) const;
--|
--|   virtual void Clear() = 0;

  procedure Clear (R : in out Ring) is abstract;
  -- Empty the ring of all items. The mark is cleared.

--|   virtual void Insert(const Item&) = 0;

  procedure Insert (R : in out Ring; Elem : Item) is abstract;
  -- If the ring is empty, set the ring's mark to designate this
  -- item. Add the item to the top of the ring; the previous top item
  -- (if any) is now located clockwise adjacent to the new item; the
  -- old item is forward of the new one.

--|   virtual void Pop() = 0;

  procedure Pop (R : in out Ring) is abstract;
  -- Remove the item from the top of the ring. The clockwise adjacent
  -- item (if any) is now designated as the ring's top. If the removed
  -- item had been marked, the ring's new top (if not empty) is now
  -- designated as marked.

--|   virtual void Rotate(BC_Direction direction = BC_kForward) = 0;

  procedure Rotate (R : in out Ring; Dir : Direction := Forward) is abstract;
  -- Rotate the top of the ring in the given direction. Rotating the
  -- ring in a forward direction moves the ring's top clockwise;
  -- rotating the ring in a reverse direction advances the ring's top
  -- counter-clockwise. The ring's mark is unaffected. If there is
  -- exactly one item in the ring, rotating either direction always
  -- returns to the same item.

--|   virtual void Mark();

  procedure Mark (R : in out Ring);
  -- Designate the item at the top of the ring (if not empty) as
  -- marked.

--|   virtual void RotateToMark();

  procedure Rotate_To_Mark (R : in out Ring);
  -- Rotate the ring so that the ring's mark is at the top.

--|
--|   virtual BC_Index Extent() const = 0;

  function Extent (R : Ring) return Natural is abstract;
  -- Return the number of items in the ring.

--|   virtual BC_Boolean IsEmpty() const = 0;

  function Is_Empty (R : Ring) return Boolean is abstract;
  -- Return True if and only if there are no items in the ring.

--|   virtual const Item& Top() const = 0;
--|   virtual Item& Top() = 0;

  function Top (R : Ring) return Item is abstract;
  -- Return a copy of the item at the top of the ring.

--|   virtual BC_Boolean AtMark() const;

  function At_Mark (R : Ring) return Boolean;
  -- Return True if and only if the item at the top of the ring is
  -- marked; otherwise, return False. By implication, this member function
  -- will return True if the ring is empty, since the ring's top and mark
  -- both do not designate any item.
  -- XXX hmm, odd logic there!

--|
--| protected:

private
--|
--|   BC_ExtendedIndex fTop;
--|   BC_ExtendedIndex fMark;

  type Ring is abstract new Container with record
    Top : Natural;      -- 0 implies not set
    Mark : Natural;     -- 0 implies not set
  end record;

  procedure Initialize (R : in out Ring); -- derivations will need to call this

  procedure Adjust (R : in out Ring);

  procedure Add (R : in out Ring; Elem : Item);

--|
--|   virtual void Purge() = 0;
--|   virtual void Add(const Item&) = 0;
--|   virtual BC_Index Cardinality() const = 0;
--|   virtual const Item& ItemAt(BC_Index) const = 0;
--|
--|   virtual void Lock();
--|   virtual void Unlock();
--|
--| private:
--|
--|   friend class BC_TRingActiveIterator<Item>;
--|   friend class BC_TRingPassiveIterator<Item>;
--|
--|   friend class BC_TPersist<Item, BC_TRing<Item> >;
--|
--| };
--|
--| // Ring iterators
--|
--| template <class Item>
--| class BC_TRingActiveIterator {
--| public:
--|
--|   BC_TRingActiveIterator(const BC_TRing<Item>&);
--|   ~BC_TRingActiveIterator();
--|
--|   void Reset();
--|   BC_Boolean Next();
--|
--|   BC_Boolean IsDone() const;
--|   const Item* CurrentItem() const;
--|   Item* CurrentItem();
--|
--| protected:
--|
--|   const BC_TRing<Item>& fRing;
--|   BC_ExtendedIndex fIndex;
--|
--| };
--|
--| template <class Item>
--| class BC_TRingPassiveIterator {
--| public:
--|
--|   BC_TRingPassiveIterator(const BC_TRing<Item>&);
--|   ~BC_TRingPassiveIterator();
--|
--|   BC_Boolean Apply(BC_Boolean (*Process)(const Item&));
--|   BC_Boolean Apply(BC_Boolean (*Process)(Item&));
--|
--| protected:
--|
--|   const BC_TRing<Item>& fRing;
--|
--| };
--|
--| #endif

  type Ring_Iterator (R : access Ring'Class)
  is new Actual_Iterator (R) with record
    Index : Natural;
  end record;

  -- Overriding primitive supbrograms of the concrete actual Iterator.

  procedure Initialize (It : in out Ring_Iterator);

  procedure Reset (It : in out Ring_Iterator);

  procedure Next (It : in out Ring_Iterator);

  function Is_Done (It : Ring_Iterator) return Boolean;

  function Current_Item (It : Ring_Iterator) return Item;

  function Current_Item (It : Ring_Iterator) return Item_Ptr;

end BC.Containers.Rings;
