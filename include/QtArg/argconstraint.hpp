
/*!
	\file

	\brief Command Line Argument's Constraint.

	\author Igor Mironchik (igor.mironchik at gmail dot com).

	Copyright (c) 2010-2012 Igor Mironchik

	Permission is hereby granted, free of charge, to any person
	obtaining a copy of this software and associated documentation
	files (the "Software"), to deal in the Software without
	restriction, including without limitation the rights to use,
	copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the
	Software is furnished to do so, subject to the following
	conditions:

	The above copyright notice and this permission notice shall be
	included in all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
	EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
	OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
	NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
	HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
	WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
	FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
	OTHER DEALINGS IN THE SOFTWARE.
*/

#ifndef QTARG__ARGCONSTRAINT_HPP__INCLUDED
#define QTARG__ARGCONSTRAINT_HPP__INCLUDED

// Qt include.
#include <QtCore/QVariant>


//
// QtArgConstraintIface
//

/*!
	\class QtArgConstraintIface
	\brief Base class for the constraints of argument's values.
*/
class QtArgConstraintIface {
	protected:
		virtual ~QtArgConstraintIface() {}

	public:
		//! \retval true if constraint observed.
		//! \retval false otherwise.
		virtual bool check( const QVariant & value ) const = 0;
}; // class QtArgConstraintIface

#endif // QTARG__ARGCONSTRAINT_HPP__INCLUDED
