using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class LevelControl : MonoBehaviour
{
    public int index;
    string levelName;
    public int goal;
    // Start is called before the first frame update
    void Start()
    {
        index = SceneManager.GetActiveScene().buildIndex;
        if(index <= 1)
        {
            goal = 3;
        }
        else
        {
            goal = 5;
        }
    }

    // Update is called once per frame
    void Update()
    {
        
    }

    public void nextLevel()
    {
        index = index + 1;
        SceneManager.LoadScene(index);
        SaveLevel();
    }

    public int getLevel()
    {
        return index;
    }

    public int getGoal()
    {
        return goal;
    }

    public void SaveLevel()
    {
        SaveSystem.SaveLevel(this);
    }

    public void LoadLevel()
    {
        LevelData data = SaveSystem.LoadLevel();
        index = data.levelIndex;
        SceneManager.LoadScene(index);
    }
}
