livingThing <- function(HP=sample(1:4, 1), 
                        AC = 10)
{
        thisEnv <- environment()
        
        ##Stats
        HP <- HP
        AC <- AC
        
        ##list of methods
        me <- list(
                
                ## Define the environment where this list is defined so
                ## that I can refer to it later.
                thisEnv = thisEnv,
                
                ## Define the accessors for the data fields.
                getEnv = function()
                {
                        return(get("thisEnv",thisEnv))
                },
                
                getHP = function()
                {
                        return(get("HP",thisEnv))
                },
                
                setHP = function(value)
                {
                        return(assign("HP", value, thisEnv))
                },
                
                getAC = function()
                {
                        return(get("AC",thisEnv))
                },
                
                setAC = function(value)
                {
                        return(assign("AC",value,thisEnv))
                },
                
                takeDmg = function(Dmg)
                {
                        return(assign("HP", this$getHP() - Dmg, thisEnv))
                }
                
        )
        
        ## Define the value of the list within the current environment.
        assign('this',me,envir=thisEnv)
        
        ## Set the name for the class
        class(me) <- append(class(me),"livingThing")
        return(me)
}

hero <- function(Name,
                 HP=sample(1:10, 1), 
                 AC = 10,
                 Strength = 10, 
                 Dexterity = 10, 
                 Constitution = 10,
                 Wisdom = 10,
                 Intelligence = 10,
                 Karisma = 10)
{
        thisEnv <- environment()
        
        ##Stats
        Name <- Name
        Strength <- Strength
        Dexterity <- Dexterity
        Constitution <- Constitution
        Wisdom <- Wisdom
        Intelligence <- Intelligence
        Karisma <- Karisma
        
        ##list of methods
        me <- livingThing(HP, AC)
        
        ## Define the value of the list within the current environment.
        assign('this',me,envir=thisEnv)
        assign('this2',me,envir=thisEnv)
        
        ## Set the name for the class
        class(me) <- append(class(me),"hero")
        return(me)
}
