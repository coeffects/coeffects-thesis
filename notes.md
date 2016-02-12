> ``````````````````````````````````````````````````````````````````````````````````````````````````
>
>       ***** *     **
>    ******  **    **** *                *
>   **   *  * **    ****                **
>  *    *  *  **    * *                 **
>      *  *    **   *        ****     ********              ****
>     ** **    **   *       * ***  * ********     ***      * **** *
>     ** **     **  *      *   ****     **       * ***    **  ****
>     ** **     **  *     **    **      **      *   ***  ****
>     ** **      ** *     **    **      **     **    ***   ***
>     ** **      ** *     **    **      **     ********      ***
>     *  **       ***     **    **      **     *******         ***
>        *        ***     **    **      **     **         ****  **
>    ****          **      ******       **     ****    * * **** *
>   *  *****                ****         **     *******     ****
>  *     **                                      *****
>  *
>   **
>
> ``````````````````````````````````````````````````````````````````````````````````````````````````

# TODO

*Section 5.2.5*

Semantics of implicit parameters - this should use the "right" sort of `\cup` 
when merging parameters from declaration site and call site so that it matches
the definition from Chapter 2.

Also, add note that this works with the "unique typing derivation" chosen in 
Chapter 4.

*Chapter 2 - implicit params*

Should mention the same typing derivation as the one showed later.



# Notable changes

 - **Section 4.2** - specifies how a unique typing derivation is chosen 
   (resolving the ambiguity in the lambda abstraction for concrete coeffect systems)
   
 - **Section 5.2.6** - notation has been changed so that it is clear that
   semantics is defined over a typing derivation (the one from section 4.2)
   
 - **Theorem 16** - abstract matches concrete
 
 - translation