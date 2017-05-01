livingThing <- function(HP, Attack, Defense)
{
        
        me <- list(
                HP = HP,
                Attack = Attack,
                Defense = Defense
        )
        
        ## Set the name for the class
        class(me) <- append(class(me),"livingThing")
        return(me)
}

takeDmg <- function(obj)
{
        UseMethod("takeDmg", obj)
}

takeDmg.livingThing <- function(obj, dmg)
{
        obj$HP = obj$HP - dmg
        
}

Juggernaught = livingThing(HP = 100, Attack = 10, Defense = 10)

takeDmg(Juggernaught, 10)
