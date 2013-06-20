#include <Julian.h>
#include <Tle.h>
#include <SGP4.h>
#include <Globals.h>
#include <Util.h>
#include <Observer.h>
#include <CoordGeodetic.h>
#include <CoordTopographic.h>

#include <list>
#include <string>
#include <iomanip>
#include <iostream>
#include <fstream>
#include <vector>
#include <cstdlib>

void RunTle(Tle tle, double start, double end, double inc)
{
    double current = start;
    SGP4 model(tle);
    bool running = true;
    bool first_run = true;
    std::ofstream myfile;

    std::cout << std::setprecision(0) << tle.NoradNumber() << " xx"
        << std::endl;

    myfile.open("propagated_state_vector.txt");

    while (running)
    {
        bool error = false;
        Vector position;
        Vector velocity;
        double tsince;

        try
        {
         //   if (first_run && current != 0.0) 
         //   {
                /*
                 * make sure first run is always as zero
                 */
         //       tsince = 0.0;
         //   }
         //   else
         //   {
                /*
                 * otherwise run as normal
                 */
                tsince = current;
          //  }

            Eci eci = model.FindPosition(tsince);
            position = eci.GetPosition();
            velocity = eci.GetVelocity();
        }
        catch (SatelliteException& e)
        {
            std::cerr << e.what() << std::endl;
            error = true;
            running = false;
        }
        catch (DecayedException& e)
        {
            std::cerr << e.what() << std::endl;

            position = e.GetPosition();
            velocity = e.GetVelocity();

            if (!first_run)
            {
                // print out position on first run
                error = true;
            }

            running = false;
        }

        if (!error)
        {
            // std::cout << std::setprecision(8) << std::fixed;
            // std::cout.width(17);
            // std::cout << tsince << " ";
            // std::cout.width(16);
            // std::cout << position.x << " ";
            // std::cout.width(16);
            // std::cout << position.y << " ";
            // std::cout.width(16);
            // std::cout << position.z << " ";
            // std::cout << std::setprecision(9) << std::fixed;
            // std::cout.width(14);
            // std::cout << velocity.x << " ";
            // std::cout.width(14);
            // std::cout << velocity.y << " ";
            // std::cout.width(14);
            // std::cout << velocity.z << std::endl;
	    
            myfile << std::setprecision(8) << std::fixed;
            std::cout.width(17);
            myfile << tsince-start << " ";
            std::cout.width(16);
            myfile << position.x << " ";
            std::cout.width(16);
            myfile << position.y << " ";
            std::cout.width(16);
            myfile << position.z << " ";
            myfile << std::setprecision(9) << std::fixed;
            std::cout.width(14);
            myfile << velocity.x << " ";
            std::cout.width(14);
            myfile << velocity.y << " ";
            std::cout.width(14);
            myfile << velocity.z << std::endl;
        }

 //       if ((first_run && current == 0.0) || !first_run)
 //       {
            if (current == end)
            {
                running = false;
            }
            else if (current + inc > end)
            {
                current = end;
            }
            else
            {
                current += inc;
            }
 //       }
        first_run = false;
    }
}

void tokenize(const std::string& str, std::vector<std::string>& tokens)
{
    const std::string& delimiters = " ";

    /*
     * skip delimiters at beginning
     */
    std::string::size_type last_pos = str.find_first_not_of(delimiters, 0);

    /*
     * find first non-delimiter
     */
    std::string::size_type pos = str.find_first_of(delimiters, last_pos);

    while (std::string::npos != pos || std::string::npos != last_pos)
    {
        /*
         * add found token to vector
         */
        tokens.push_back(str.substr(last_pos, pos - last_pos));
        /*
         * skip delimiters
         */
        last_pos = str.find_first_not_of(delimiters, pos);
        /*
         * find next non-delimiter
         */
        pos = str.find_first_of(delimiters, last_pos);
    }
}

extern "C" {

void propagate_state_vector(const char* infile)
{
    std::ifstream file;

    file.open(infile);

    if (!file.is_open())
    {
        std::cerr << "Error opening file" << std::endl;
        return;
    }
    else
    {
        std::cout << "Infile " << infile << " opened" << std::endl;;
    }
    
    bool got_first_line = false;
    std::string line1;
    std::string line2;
    std::string parameters;

    while (!file.eof())
    {
        std::string line;
        std::getline(file, line);

        Util::Trim(line);

        /*
         * skip blank lines or lines starting with #
         */
        if (line.length() == 0 || line[0] == '#')
        {
            got_first_line = false;
            continue;
        }

        /*
         * find first line
         */
        if (!got_first_line)
        {
            try
            {
                if (line.length() >= Tle::GetLineLength())
                {
                    Tle::IsValidLine(line.substr(0, Tle::GetLineLength()), 1);
                    /*
                     * store line and now read in second line
                     */
                    got_first_line = true;
                    line1 = line;
                }
            }
            catch (TleException& e)
            {
                std::cerr << "Error: " << e.what() << std::endl;
                std::cerr << line << std::endl;
            }
        }
        else
        {
            /*
             * no second chances, second line should follow the first
             */
            got_first_line = false;
            /*
             * split line, first 69 is the second line of the tle
             * the rest is the test parameters, if there is any
             */
            line2 = line.substr(0, Tle::GetLineLength());
            double start = 0.0;
            double end = 1440.0;
            double inc = 120.0;
            if (line.length() > 69)
            {
                std::vector<std::string> tokens;
                parameters = line.substr(Tle::GetLineLength() + 1,
                        line.length() - Tle::GetLineLength());
                tokenize(parameters, tokens);
                if (tokens.size() >= 3)
                {
                    start = atof(tokens[0].c_str());
                    end = atof(tokens[1].c_str());
                    inc = atof(tokens[2].c_str());
                }
            }

            /*
             * following line must be the second line
             */
            try
            {
                if (line.length() >= Tle::GetLineLength())
                {
                    Tle::IsValidLine(line.substr(0, Tle::GetLineLength()), 2);
                    Tle tle("Test", line1, line2);
                    RunTle(tle, start, end, inc);
                }
            }
            catch (TleException& e)
            {
                std::cerr << "Error: " << e.what() << std::endl;
                std::cerr << line << std::endl;
            }
        }
    }

    /*
     * close file
     */
    file.close();

    return;
}
}
