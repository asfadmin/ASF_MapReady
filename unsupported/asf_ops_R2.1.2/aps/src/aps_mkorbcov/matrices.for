
*.
*   Matrices - 3D matrix operations
*
*	mSet - set the values of a 3x3 matrix
*	mSetRows - set the values of a 3x3 matrix using 3 row vectors
*	mSetCols - set the values of a 3x3 matrix using 3 column vectors
*	mTranspose - find transpose of a 3x3 matrix
*	mMult3x1 - premultiply a 3x1 (column) matrix by a 3x3 matrix
*	mMult3x3 - multiply two 3x3 matrices
*
*	08/04/89 13:32
*..

	subroutine mSet(a,a11,a12,a13,a21,a22,a23,a31,a32,a33)
	double precision a(3,3)
	double precision a11,a12,a13,a21,a22,a23,a31,a32,a33

	  character*100 SccsFileID
     -/'@(#)matrices.for	5.1 98/01/08 APS/ASF\0'/
	a(1,1) = a11
	a(1,2) = a12
	a(1,3) = a13

	a(2,1) = a21
	a(2,2) = a22
	a(2,3) = a23

	a(3,1) = a31
	a(3,2) = a32
	a(3,3) = a33

	end


	subroutine mSetRows(a,v1,v2,v3)
	double precision a(3,3)
	double precision v1(3),v2(3),v3(3)

	a(1,1) = v1(1)
	a(1,2) = v1(2)
	a(1,3) = v1(3)

	a(2,1) = v2(1)
	a(2,2) = v2(2)
	a(2,3) = v2(3)

	a(3,1) = v3(1)
	a(3,2) = v3(2)
	a(3,3) = v3(3)

	end


	subroutine mSetCols(a,v1,v2,v3)
	double precision a(3,3)
	double precision v1(3),v2(3),v3(3)

	a(1,1) = v1(1)
	a(2,1) = v1(2)
	a(3,1) = v1(3)

	a(1,2) = v2(1)
	a(2,2) = v2(2)
	a(3,2) = v2(3)

	a(1,3) = v3(1)
	a(2,3) = v3(2)
	a(3,3) = v3(3)

	end


	subroutine mTranspose(a,b)
	double precision a(3,3),b(3,3)

	integer i,j

	do 20 j=1,3
	    do 10 i=1,3
		b(i,j) = a(j,i)
10	    continue
20	continue

	end


	subroutine mMult3x1(a,x,y)
	double precision a(3,3),x(3),y(3)

	integer j

	do 10 j=1,3
	    y(j) = a(j,1) * x(1) + a(j,2) * x(2) + a(j,3) * x(3)
10	continue

	end


	subroutine mMult3x3(a,b,c)
	double precision a(3,3),b(3,3),c(3,3)

	integer i,j

	do 20 j=1,3
	    do 10 i=1,3
		c(i,j) = a(i,1) * b(1,j) + a(i,2) * b(2,j) + a(i,3) * b(3,j)
10	    continue
20	continue

	end
