let rec buildTree = (preorder, inorder) => {

	switch (preorder, inorder) {

            | ([], []) => EmptyTree

      	    | ([headPreorder, ...tailPreorder], inorder) => {

      		let root = headPreorder

          	let rootIndex = List.indexOf(inorder, root);

          	let leftInorder = List.take(inorder, rootIndex);

          	let rightInorder = List.drop(inorder, rootIndex + 1);

          	let leftPreorder = List.take(tailPreorder, List.length(leftInorder));

          	let rightPreorder = List.drop(tailPreorder, List.length(leftInorder));

          	TreeNode(buildTree(leftPreorder, leftInorder), root, buildTRee(rightPreorder, rightInorder))

      }

  }

};



let tree = buildTree([2,1,3], [1,2,3]);

/*

TreeNode(

	TreeNode(EmptyTree, 1, EmptyTree),

    2,

    TreeNode(EmptyTree, 3, EmptyTree)

)

*/
